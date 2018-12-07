library(pbapply)
library(dplyr)

#Read the survey
df.raw <- read.csv("Data/Automation_sheet.csv")
df.raw <- df.raw %>% mutate(COD_OCUPACAO = as.numeric(CBO_5D))

#Data transformation
df.raw <- df.raw %>% 
  mutate(Prob = Prob/100) %>% 
  mutate(Prob = case_when(Prob == 0 ~ 1e-5,
                          Prob == 1 ~ 1-1e-5,
                          TRUE ~ Prob)) %>% 
  mutate(Score = log(Prob/(1-Prob)))

#Distinct CBO data
df.raw <- df.raw %>% 
  group_by(COD_OCUPACAO) %>% 
  summarise(Score = mean(Score))

#Read the data
dataWords<-readRDS("Data/CBOPCAwords.rds")

#70% of variance explained
dataWords<-dataWords[,1:373]    #373

#Full join
df.full <- full_join(df.raw, dataWords, by="COD_OCUPACAO")
df <- df.full[which(!is.na(df.full$Score)),]
df <- df %>% 
  group_by_at(names(df)[3:ncol(df)]) %>% 
  summarise(Score = mean(Score))

#Zero Mean data
full <- scale(df, scale = FALSE)
#Explanatory variables
X <- as.matrix(full[,-ncol(df)])
Y <- full[,ncol(df)]

# Gaussian Process --------------------------------------------------------
Rcpp::sourceCpp("Analysis/Kernel.cpp")

system.time(
  a <- KernelMatrix2(X,X,2.5,1.2,as.matrix(rep(1,ncol(X))))
)
system.time( 
  b <- IBS_kernel_C_parallel(X, as.matrix(2.5), as.matrix(1.2), as.matrix(1)) 
)
a[1:5,1:5]
b[1:5,1:5]

marginal.ll_<-function(theta){
  #Parameters
  n <- nrow(X)
  sigma2f <- as.matrix(theta[1])
  sigma.n <- as.matrix(theta[2])
  lambda  <- as.matrix(theta[3])
  #Kernel
  k.xx_ <- IBS_kernel_C_parallel(X, sigma2f, sigma.n, lambda)
  # k.xx <- as.matrix(Matrix::nearPD(k.xx)$mat)
  L_ <-  chol(k.xx_)
  invL_ <- backsolve(r = L_, x = diag(ncol(L_)))
  loglik <- -.5*t(Y)%*%t(invL_)%*%invL_%*%Y - .5*2*sum(log(diag(L_))) - n*.5*log(2*pi)
  
  return(loglik)
}
system.time( marginal.ll_(rep(1,3)) )   # About 2 seconds


# First Grid --------------------------------------------------------------
n.par<-3
initialPop<-2^seq(-7,10)
final <- vector("list", n.par) 
for(p in 1:n.par){
  final[[p]]<-initialPop
}

initialPop<-do.call(expand.grid, final)
initialPop<-as.data.frame(initialPop)

nrow(initialPop)*2/(60*60) # 4 hours

#   -----------------------------------------------------------------------
start_time <- Sys.time()
start_time

op <- pboptions(type = "timer") # default
pboptions(type = "txt", style = 1, char = "=")

res <- pbapply(initialPop, 1, marginal.ll_)

end_time <- Sys.time()
end_time - start_time

summary(res)
q99 <- quantile(res, .999)
cbind(initialPop[which(res>q99),], "logll" = res[which(res>q99)])

# Best choice (8,4,8)
saveRDS(res, "res_mean.RDS")

# Second Grid -------------------------------------------------------------
p1 <- seq(4,16, by = .25)
p2 <- seq(2, 8, by = .25)

final <- vector("list", 3)
final[[1]] <- final[[3]] <- p1
final[[2]] <- p2

initialPop<-do.call(expand.grid, final)
initialPop<-as.data.frame(initialPop)

nrow(initialPop)*2/(60*60) # 33.35 hours

##
start_time <- Sys.time()
start_time

op <- pboptions(type = "timer") # default
pboptions(type = "txt", style = 1, char = "=")

res2 <- pbapply(initialPop, 1, marginal.ll_)

end_time <- Sys.time()
end_time - start_time

summary(res2)
q99 <- quantile(res2, .999)
cbind(initialPop[which(res2>q99),], "logll" = res2[which(res2>q99)])

saveRDS(res2, "res_second_grid.RDS")
res2 <- readRDS("res_second_grid.RDS")
# Otimal Value
cbind(initialPop[which.max(res2),], "logll" = res2[which.max(res2)])

# Prediction Data ---------------------------------------------------------
theta <- as.numeric(initialPop[which.max(res2),])
# theta <- c(10.5, 2, 5.5)

# prediction
pred.gp <- function(x.star, theta){
  X <- as.matrix(X)
  x.star <- as.matrix(x.star)
  k.xx <- IBS_kernel_C_parallel(X, as.matrix(theta[1]), as.matrix(theta[2]), as.matrix(theta[3]))
  k.xsx <- KernelMatrix2(x.star, X, sigma2f = theta[1], sigma2n = 0,
                         theta = rep(theta[3], ncol(X)))
  k.xsxs <- KernelMatrix2(x.star, x.star, sigma2f = theta[1], sigma2n = 0,
                          theta = rep(theta[3], ncol(X)))
  L <-  chol(k.xx)
  invL <- backsolve(r = L, x = diag(ncol(L)))
  invk <-  t(invL)%*%invL
  
  mu.hat <- k.xsx %*% invk %*% Y
  Sigma.hat <- k.xsxs - k.xsx %*% invk %*% t(k.xsx)
  return(list(pred = mu.hat, Sigma.hat = Sigma.hat))
}

pred <- pred.gp(X, theta)
plot(pred$pred, Y)
abline(coef = c(0,1))
mean((pred$pred-Y)^2)


# Predict all data --------------------------------------------------------
df.raw <- read.csv("Data/Automation_sheet.csv")
df.raw <- df.raw %>% mutate(COD_OCUPACAO = as.numeric(CBO_5D))
#Data transformation
df.raw <- df.raw %>% 
  mutate(Prob = Prob/100) %>% 
  mutate(Prob = case_when(Prob == 0 ~ 1e-5,
                          Prob == 1 ~ 1-1e-5,
                          TRUE ~ Prob)) %>% 
  mutate(Score = log(Prob/(1-Prob)))
#Distinct CBO data
df.raw <- df.raw %>% 
  group_by(COD_OCUPACAO) %>% 
  summarise(Prob = mean(Prob), 
            var = var(Score),
            Score = mean(Score)) %>% 
  mutate(var = ifelse(is.na(var), 0, var))
#Read the data
dataWords<-readRDS("Data/CBOPCAwords.rds")
#70% of variance explained
dataWords<-dataWords[,1:373]    #373
#Full join
df.full <- full_join(df.raw, dataWords, by="COD_OCUPACAO")

#### Escalonando pelas medias de treinamento
medias <- apply(as.data.frame(df[-ncol(df)]), 2, mean)
x.star <- t(apply(df.full[,-(1:4)], 1, function(x)x-medias))

pred_x.star <- pred.gp(x.star, theta)

######
compara <- df.full %>% 
  mutate(Pred_Score = pred_x.star$pred,
         Pred_Prob = exp(Pred_Score + mean(df$Score))/
           (1 + exp(Pred_Score + mean(df$Score))) ) %>% 
  dplyr::select(COD_OCUPACAO, Prob, Pred_Prob, Score, Pred_Score)

plot(ecdf(compara$Prob), verticals=TRUE, do.points=FALSE, col="blue") 
plot(ecdf(compara$Pred_Prob), verticals=TRUE, do.points=FALSE, col="red", add = T) 

max(abs(ecdf(compara$Prob)(compara$Prob) - ecdf(compara$Pred_Prob)(compara$Pred_Prob)), na.rm = T)
ecdf(compara$Prob)(compara$Prob)[125:130]
ecdf(compara$Pred_Prob)(compara$Pred_Prob)[125:130]
######

# Simulate CBO ------------------------------------------------------------
set.seed(2309)
n.sim <- 5000
sim.X <- t(MASS::mvrnorm(n.sim, as.numeric(pred_x.star$pred), 
                   as.matrix(pred_x.star$Sigma.hat)))
res.X <-data.frame("COD_OCUPACAO"=df.full$COD_OCUPACAO,sim.X)

#Recalculate the probability
res.X[,2:(n.sim+1)] <- res.X[,2:(n.sim+1)] + mean(df$Score)
res.X[,2:(n.sim+1)] <- exp(res.X[,2:(n.sim+1)])/(1+exp(res.X[,2:(n.sim+1)]))
res.X$Sum <- rowSums(res.X[,2:(n.sim+1)])
res.sum <- res.X  %>% 
  group_by(COD_OCUPACAO) %>% 
  summarise(Sum=sum(Sum),
            n=n())
res.X<-res.X[,-ncol(res.X)]
res.sum$Prob<-(res.sum$Sum/(res.sum$n*n.sim))
res.X <- full_join(res.sum,res.X,by="COD_OCUPACAO")
res.X <- res.X[,c(-2,-3)]

#Final data
final.df<-full_join(res.X[,c(1,2)], df.full[,c(1,2)],by="COD_OCUPACAO")
plot(final.df$Prob.x,final.df$Prob.y)

#Export dataset
temp <-res.X
temp$Count<-1
library(dplyr)
temp2<-temp[,-2]
export <- temp2 %>% group_by(COD_OCUPACAO) %>%  mutate(cum = cumsum(Count))
export$ID<-paste0(export$COD_OCUPACAO,"_",export$cum)

export2<-export[,c(-1,-5003,-5002)]

export3 <- reshape2::melt(export2, id.vars ="ID")
export3$COD_OCUPACAO<-sapply(strsplit(export3$ID,"_"), `[`, 1)
export3<-export3[,c(-1,-2)]
export3<-export3[,c(2,1)]
colnames(export3)[2]<-"Probability"
export3 <- export3[order(export3$COD_OCUPACAO),] 
export3$Probability<-round(export3$Probability,5)

saveRDS(export3,"ProbabilityGP.rds")
write.csv(export3,"ProbabilityGP.csv")
save.image("AnalysisObjects.RData")

# with rounded numbers 
export4 <- data.frame(COD_OCUPACAO = export3$COD_OCUPACAO, 
                      Probability = round(export3$Probability,4) )
saveRDS(export4,"ProbabilityGP_round.rds")



# Check few CBO
hist(export4[which(export4$COD_OCUPACAO == "214720"),2], 50) # low trained prob
hist(export4[which(export4$COD_OCUPACAO == "724435"),2], 50) # low non-trained prob

hist(export4[which(export4$COD_OCUPACAO == "234770"),2], 50) # near .5 trained prob
hist(export4[which(export4$COD_OCUPACAO == "234710"),2], 50) # near .5 non-trained prob

hist(export4[which(export4$COD_OCUPACAO == "239420"),2], 50) # high trained prob
hist(export4[which(export4$COD_OCUPACAO == "239415"),2], 50) # high non-trained prob
hist(export4[which(export4$COD_OCUPACAO == "376215"),2], 50) # high non-trained prob


hist(diag(pred_x.star$Sigma.hat), 500)
summary(diag(pred_x.star$Sigma.hat))

a <- export4 %>% 
  group_by(COD_OCUPACAO) %>% 
  summarise(prob = median(Probability),
            var = var(Probability))

summary(a$var)
hist(export4[which(export4$COD_OCUPACAO == "862510"),2], 50) # lowest variance
hist(export4[which(export4$COD_OCUPACAO == "612310"),2], 50) # lowest variance

hist(export4[which(export4$COD_OCUPACAO == "322210"),2], 50) # highest variance
hist(export4[which(export4$COD_OCUPACAO == "721425"),2], 50) # highest variance

