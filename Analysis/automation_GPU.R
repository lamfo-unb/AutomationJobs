library(tidyverse)
library(MASS)
library(gpuR)
library(pbapply)
set.seed(3938)

df <- readRDS("df.RDS")

#Zero Mean data
full<- scale(df, scale = FALSE)

#Explanatory variables
X <- as.matrix(full[,-ncol(df)])
Y <- full[,ncol(df)]

n.par <- 3
theta <- rep(1,n.par)
#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
  tryCatch({
    #Parameters
    sigma2f <- theta[1]
    sigma2.n <- theta[2]
    lambda  <- theta[3]
    n <- nrow(X)
    Y.gpu <- vclVector(Y)  
    
    #X.gpu <- vclMatrix(X, type='float')
    X.g <- X*lambda
    X.gpu <- vclMatrix(X.g)
    k.xx <- suppressWarnings(gpuR::dist(X.gpu,method="sqEuclidean"))
    k.xx <- sigma2f*exp(-0.5*k.xx)
    k.xx <- k.xx + sigma2.n*diag(1, n)
    L <-  chol(k.xx)
    logdet <- 2*sum(log(as.matrix(diag(L))))
    #Regularization
    invL <- solve(L)
    invK <- t(invL)%*%invL        
    #Information
    k1<-invK%*%Y.gpu
    k1<-as.numeric(as.matrix(k1%*%Y.gpu))
    #Information
    inf <- as.numeric((-0.5)*(k1))
    if(is.infinite(logdet) | is.na(logdet) | is.nan(logdet)){
      return(NA)
    }
    else{
      reg <- as.numeric((-0.5)*logdet)
      #Normalization
      nor <- -(n/2)*log(2*pi)
    }
    #Return Log-likelihood
    return(inf+reg+nor)
  },
  error=function(e) {
    NA
  })
}

n.par<-3
#initialPop<-eval(seq(0.001,100,length.out = gri))
initialPop <- 2^seq(-3,5)
final <- vector("list", n.par) 
for(p in 1:n.par){
  final[[p]]<-initialPop
}
#hours <- (2.5*(gri^n.par))/1000
initialPop <- do.call(expand.grid, final)
initialPop <- as.data.frame(initialPop)

#####
initialPop <- initialPop[which(initialPop$Var3 < 2),]
#####

(start_time <- Sys.time())

op <- pboptions(type = "timer") # default
pboptions(type = "txt", style = 1, char = "=")
res <- pbapply(initialPop, 1, marginal.ll)

end_time <- Sys.time()
end_time - start_time


###################################
# save.image("Solution_12-11.RData")
load("Solution_12-11.RData")
###################################

summary(res)

q99 <- quantile(res,.99)
cbind(initialPop[which(res>q99),], "ll" = res[which(res>q99)])

(theta<-initialPop[which(res==max(res)),])
res[ind]
###################################
marginal.ll(unlist(theta))
marginal.ll(c(8,4,.25))

marginal.ll(c(resa$par[1:2], 1/resa$par[3]))
marginal.ll(c(0.03125,0.03125,32))
marginal.ll(c(32,32,32))
marginal.ll(theta.hat)
###################################

# Prediction --------------------------------------------------------------
# source("pred_gpu.R")
Rcpp::sourceCpp("kernel.cpp")

pred.gp <- function(x.star, theta){
  X <- as.matrix(X)
  x.star <- as.matrix(x.star)
  k.xx <- KernelMatrix2(X,X, theta[1], theta[2], rep(theta[3], ncol(x.star)))
  k.xsx <- KernelMatrix2(x.star, X, sigma2f = theta[1], sigma2n = 0,
                         theta =  rep(theta[3], ncol(x.star)))
  # k.xx <- as.matrix(Matrix::nearPD(k.xx)$mat)
  k.xsxs <- KernelMatrix2(x.star, x.star, sigma2f = theta[1], sigma2n = 0,
                          theta =  rep(theta[3], ncol(x.star)))
  
  mu.hat <- k.xsx %*% solve(k.xx) %*% Y
  Sigma.hat <- k.xsxs - k.xsx %*% solve(k.xx) %*% t(k.xsx)
  return(list(pred = mu.hat, Sigma.hat = Sigma.hat))
}

pred <- pred.gp(X, as.numeric(theta))
plot(pred$pred, Y)
abline(coef = c(0,1))
mean((pred$pred-Y)^2)
plot(pred$pred-Y)
summary(diag(pred$Sigma.hat))


y_hat <- pred$pred + mean(df$Score)
y_hat <- exp(y_hat)/(1+exp(y_hat))
summary(y_hat)

y <- Y + mean(df$Score)
y <- exp(y)/(1+exp(y))
summary(y)

plot(y_hat, y)

pred$Sigma.hat[1:5, 1:5]

i <- which(pred$pred == min(pred$pred))
j <- which(pred$pred == max(pred$pred))

p <- rnorm(1000 ,pred$pred[i], pred$Sigma.hat[i,i])
p <- p + mean(df$Score)
p <- exp(p)/(1+exp(p))
hist(p, 50)
mean(p)
median(p)

p <- rnorm(1000 ,pred$pred[j], pred$Sigma.hat[j,j])
p <- p + mean(df$Score)
p <- exp(p)/(1+exp(p))
hist(p, 50)
mean(p)
median(p)

#Simulate values
df.raw <- read.csv("Automation_sheet.csv")
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
dataWords<-readRDS("CBOPCAwords.rds")
#70% of variance explained
dataWords<-dataWords[,1:373]    #373
#Full join
df.full <- full_join(df.raw, dataWords, by="COD_OCUPACAO")
#### Escalonando pelas medias de treinamento
medias <- apply(as.data.frame(df[-ncol(df)]), 2, mean)
x.star <- t(apply(df.full[,-(1:4)], 1, function(x)x-medias))
#### # x.star <- scale(df.full[,-(1:4)] ,scale = FALSE)

pred_x.star <- pred.gp(x.star, as.numeric(theta))

######
compara <- df.full %>% 
  mutate(Pred_Score = pred_x.star$pred,
         Pred_Prob = exp(Pred_Score + mean(df$Score))/
           (1 + exp(Pred_Score + mean(df$Score))) ) %>% 
  dplyr::select(COD_OCUPACAO, Prob, Pred_Prob, Score, Pred_Score)
######

n.sim <- 5000
sim.X <- t(mvrnorm(n.sim, as.numeric(pred_x.star$pred), 
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
