library(tidyverse)
library(kergp)
library(earth)
library(RcppEigen)
library(RcppParallel)
library(MASS)
library(tidyverse)
library(gpuR)
library(BB)
library(dfoptim)
library(pso)
library(RcppDE)
set.seed(3938)
#Read the survey
df.raw <- read.csv("Data\\Automation_sheet.csv")
colnames(df.raw)[2]<-"COD_OCUPACAO"
df.raw$COD_OCUPACAO<-as.numeric(as.character(df.raw$COD_OCUPACAO))
df.raw <- df.raw[!is.na(df.raw$COD_OCUPACAO),]

#Read the data
dataWords<-readRDS("Data/CBOPCAwords.rds")

#Inner join
df.full<-full_join(df.raw,dataWords,by="COD_OCUPACAO")
df<-df.full[which(!is.na(df.full$Prob)),]
df.star<-df.full
#Remove columns with zero
df<-na.omit(df)
#ind<-which(colSums(df[,-1])==0)+1
#df<-df[,-ind]
#df.star<-df.star[,-ind]

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
nc <-ncol(full)
X <- as.matrix(full[,-nc])
Y<- full[,nc]
X.star <-as.matrix(full.star)
#X<-X[1:200,1:100]
#X.star<-X.star[1:300,1:100]
#Y<-Y[1:200]
#Compile RcppEigen
Rcpp::sourceCpp("Analysis/Kernel.cpp")

#List all context GPU
listContexts()

# set second context
setContext(1L)


#X<-X[1:200,1:100]
#X.star<-X.star[1:300,1:100]
#Y<-Y[1:200]




#Teste
#Rcpp::sourceCpp("Analysis/Kernel.cpp")
#X<-matrix(rnorm(50),ncol=5)
#lambdaVec<-seq(1,5)
#sigma2f<-0.5
#kernel1 <- sigma2f*exp(IBS_kernel_C_parallel( X, matrix(lambdaVec,ncol=1)))
#kernel2 <- KernelMatrix(X,lambdaVec,sigma2f)

#X.gpu <- gpuMatrix(X)
#lambdaVec <- gpuMatrix(diag(1/lambdaVec))
#X.gpu <- X.gpu %*% lambdaVec
#kernel3 <- gpuR::distance(X.gpu,X.gpu, method="sqEuclidean")
#kernel3 <- sigma2f*exp(-0.5*kernel3)
#kernel3 <- as.matrix(kernel3)

#all(kernel1==kernel3)

#Y.gpu <- vclVector(Y, type = "float")
n.par<-ncol(X)+2
theta<-rep(50,n.par)
#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
  #Parameters
  sigma2f <- theta[1]
  sigma.n <- theta[2]
  lambda  <- theta[3:length(theta)]
  n <- nrow(X)
  
  #GPU
  ### start_time <- Sys.time()
  lambdaMat <- vclMatrix(diag(1/lambda))
  X.gpu <- vclMatrix(X)
  X.gpu <- X.gpu %*% lambdaMat
  k.xx <- suppressWarnings(gpuR::distance(X.gpu,X.gpu, method="sqEuclidean"))
  k.xx <- sigma2f*exp(-0.5*k.xx)
  #k.xx <- as.matrix(k.xx)

  
  #Kernel
  #start_time <- Sys.time()
  #k.xx <- sigma2f*exp(IBS_kernel_C_parallel( X, matrix(lambda,ncol=1)))
  #end_time <- Sys.time()
  #end_time - start_time
  
  #start_time <- Sys.time()
  #k.xx <- KernelMatrix(X,lambda,sigma2f)
  #end_time <- Sys.time()
  #end_time - start_time
  
  k.xx <- k.xx + sigma.n^2*diag(1, n)
  L <- chol(k.xx)
  logdet <- 2*sum(log(as.matrix(diag(L))))
  #Information
  inf <- as.numeric((-0.5)*(t(Y)%*%as.matrix(k.xx)%*%Y))
  #Regularization
  det <- det(k.xx)
  
  ### end_time <- Sys.time()
  ### end_time - start_time
  if(is.infinite(logdet)){
    return(+1e+10)
  }
  else{
    reg <- as.numeric((-0.5)*logdet)
    #Normalization
    nor <- -(n/2)*log(2*pi)
  }
  #Return (Maximize)
  return(-(inf+reg+nor))
}

start_time <- Sys.time()
res <- optim(theta, marginal.ll, method = "BFGS", lower=rep(0.001,n.par), 
                     upper=rep(100,n.par), control=list(reltol=1e-1, maxit=1, trace = T))
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
res <- dfoptim::nmkb(theta, marginal.ll,lower=rep(0.001,n.par), 
                     upper=rep(100,n.par), control=list(maxfeval=2, tol=1e-01, trace=T))
end_time <- Sys.time()
end_time - start_time


res <- RcppDE::DEoptim(marginal.ll,lower=rep(0.001,n.par), upper=rep(100,n.par), DEoptim.control(NP = 50, itermax = 100, CR = 0.5, F = 0.8, trace = TRUE))


help("BBsolve")
res <- spg(theta,marginal.ll,lower=rep(0.001,n.par), upper=rep(100,n.par), control=list(maxit=100, trace=TRUE))

res <- BBsolve(theta,marginal.ll) 
#res<- optim(theta,marginal.ll,lower=rep(0.001,n.par), upper=rep(100,n.par),method="L-BFGS-B",control=list(maxit=100, trace=TRUE))
res<- optim(theta,marginal.ll,method="CG",control=list(maxit=100, trace=TRUE))	
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
save.image("Data\\AnalysisObjects.RData")
