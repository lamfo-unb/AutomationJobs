library(tidyverse)
library(RcppEigen)
library(MASS)

#Read the survey
df.raw <- read.csv("Data\\Automation_sheet.csv")
colnames(df.raw)[2]<-"COD_OCUPACAO"
df.raw$COD_OCUPACAO<-as.numeric(as.character(df.raw$COD_OCUPACAO))
df.raw <- df.raw[!is.na(df.raw$COD_OCUPACAO),]

#Read the data
dataWords<-readRDS("Data/CBOwords.rds")

#Inner join
df.full<-full_join(df.raw,dataWords,by="COD_OCUPACAO")
df<-df.full[which(!is.na(df.full$Prob)),]
df.star<-df.full
#Remove columns with zero
df<-na.omit(df)
ind<-which(colSums(df[,-1])==0)+1
df<-df[,-ind]
df.star<-df.star[,-ind]

#Data transformation
df$Prob<-df$Prob/100
df$Prob[df$Prob==0]<-1e-5
df$Prob[df$Prob==1]<-1-1e-5
df$Score<-log(df$Prob/(1-df$Prob))

#Zero Mean data
full<-df[,c(-1,-2,-3)]
full<- scale(full, scale = FALSE)
full.star<-df.star[,c(-1,-2,-3)]
full.star<- scale(full.star, scale = FALSE)

#Explanatory variables
X <- as.matrix(full[,-8360])
Y<- full[,8360]
X.star <-as.matrix(full.star)
#X<-X[1:200,1:100]
#X.star<-X.star[1:300,1:100]
#Y<-Y[1:200]
#Compile RcppEigen
Rcpp::sourceCpp("Analysis/Kernel.cpp")

#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
  #Parameters
  sigma2f <- theta[1]
  sigma.n <- theta[2]
  lambda  <- theta[3:length(theta)]
  n <- nrow(X)
  #Kernel
  k.xx <- KernelMatrix(X,lambda,sigma2f)
  k.xx <- k.xx + sigma.n^2*diag(1, n)
  #k.xx <- as.matrix(Matrix::nearPD(k.xx)$mat)
  L <- chol(k.xx)
  logdet <- 2*sum(log(diag(L)))
  #Information
  inf <- as.numeric((-0.5)*(t(Y)%*%k.xx%*%Y))
  #Regularization
  det <- det(k.xx)
  if(is.infinite(logdet)){
    return(-1e+10)
  }
  else{
    reg <- as.numeric((-0.5)*logdet)
    #Normalization
    nor <- -(n/2)*log(2*pi)
  }
  #Return (Maximize)
  return(-(inf+reg+nor))
}
n.par<-ncol(X)+2


start.time <- Sys.time()

res<- optim(rep(1,n.par),marginal.ll,lower=rep(0.001,n.par), upper=rep(1000,n.par),method="L-BFGS-B",control=list(maxit=10000))	

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

theta<-res$par

#Parameters            
sigma2f <- theta[1]
sigma.n <- theta[2]
lambda  <- theta[3:length(theta)]

#Kernel Computation
k.xx <- KernelMatrix(X,lambda,sigma2f)
k.xxs <- KernelMatrixXX(X,X.star,lambda,sigma2f)
k.xsx <- t(k.xxs)
k.xsxs <- KernelMatrix(X.star,lambda,sigma2f)

# These matrix calculations correspond to equation (2.19)
# in the book.
k.xx <- as.matrix(Matrix::nearPD(k.xx)$mat)
inv <- solve(k.xx + sigma.n^2*diag(1, ncol(k.xx)))

# Recalculate the mean and covariance functions
f.bar.star <- k.xsx%*%inv%*%Y
cov.f.star <- k.xsxs - k.xsx%*%inv%*%k.xxs

#Simulate values
n.sim<-100
sim.X <- t(mvrnorm(n.sim, f.bar.star, cov.f.star))
res.X <-data.frame("COD_OCUPACAO"=df.star$COD_OCUPACAO,sim.X)

#Recalculate the probability
res.X[,2:(n.sim+1)] <- res.X[,2:(n.sim+1)] + mean(df$Score)
res.X[,2:(n.sim+1)] <- exp(res.X[,2:(n.sim+1)])/(1+exp(res.X[,2:(n.sim+1)]))
res.X$Sum <- rowSums(res.X[,2:(n.sim+1)])
res.sum <- res.X  %>% 
           group_by(COD_OCUPACAO) %>% 
           summarise(Sum=sum(Sum),
                     n=n())
res.X<-res.X %>% select(-Sum)
res.sum$Prob<-(res.sum$Sum/(res.sum$n*n.sim))
res.X <- full_join(res.sum,res.X,by="COD_OCUPACAO")
res.X <- res.X %>%  select(-n,-Sum)

#Final data
final.df<-full_join(res.X[,c(1,2)],df[,c(2,3)],by="COD_OCUPACAO")
plot(final.df$Prob.x,final.df$Prob.y)

#Export dataset
temp <-res.X
temp$Count<-1
export <- temp %>% select(-Prob) %>% group_by(COD_OCUPACAO) %>%  mutate(cum = cumsum(Count))
export$ID<-paste0(export$COD_OCUPACAO,"_",export$cum)
export<- export %>% 
         ungroup() %>% 
         select(-COD_OCUPACAO, -Count, -cum) %>% 
         group_by(ID) %>% 
         gather(ID)
export$COD_OCUPACAO<-sapply(strsplit(export$ID,"_"), `[`, 1)
export<-export[,c(-1,-2)]
export<-export[,c(2,1)]
colnames(export)[2]<-"Probability"
