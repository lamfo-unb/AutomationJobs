library(tidyverse)
library(kergp)
library(earth)
library(RcppEigen)
library(RcppParallel)
library(MASS)
library(tidyverse)
library(GA)
library(DEoptim)
library(parallel)
library(parallelDist)
set.seed(3938)
#Read the survey
df.raw <- read.csv("Data\\Automation_sheet.csv")
colnames(df.raw)[2]<-"COD_OCUPACAO"
df.raw$COD_OCUPACAO<-as.numeric(as.character(df.raw$COD_OCUPACAO))
df.raw <- df.raw[!is.na(df.raw$COD_OCUPACAO),]

#Read the data
dataWords<-readRDS("Data/CBOPCAwords.rds")

#70% of variance explained
dataWords<-dataWords[,1:373]

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


#Teste
#X<-matrix(rnorm(50),ncol=5)
#lambdaVec<-rep(1,5)
#sigma2f<-0.5
#kernel1 <- sigma2f*exp(IBS_kernel_C_parallel( X, matrix(lambdaVec,ncol=1)))
#kernel2 <- KernelMatrix(X,lambdaVec,sigma2f)
#k<-as.matrix(parDist(X, method = "euclidean"))
#kernel3 <- sigma2f*exp(-0.5*k^2)
#all(kernel1==kernel3)

n.par<-3
theta<-rep(1,n.par)
#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
  #Parameters
  sigma2f <- theta[1]
  sigma.n <- theta[2]
  lambda  <- theta[3]
  n <- nrow(X)

  k.xx <- as.matrix(dist(X/lambda, method = "euclidean"))
  #k.xx <- as.matrix(parDist(X/lambda, method = "euclidean"))
  k.xx <- k.xx^2
  k.xx <- sigma2f*exp(-0.5*k.xx)
  
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
  #k.xx <- as.matrix(Matrix::nearPD(k.xx)$mat)
  L <- chol(k.xx)
  logdet <- 2*sum(log(diag(L)))
  #Information
  inf <- as.numeric((-0.5)*(t(Y)%*%k.xx%*%Y))
  #Regularization
  det <- det(k.xx)
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
#n.par<-3
initialPop<-eval(seq(0.001,100,length.out = 100))
final <- vector("list", n.par) 
for(p in 1:n.par){
  final[[p]]<-initialPop
}
#2^374 colunas
initialPop<-do.call(expand.grid, final)
initialPop<-as.data.frame(initialPop)
# Calculate the number of cores
no_cores <- detectCores() 

# Initiate cluster
cl <- makeCluster(no_cores)
clusterExport(cl=cl,varlist=c("X","Y")) 
start_time <- Sys.time()
  res <- parApply(cl = cl, X=initialPop, MARGIN=1, FUN=marginal.ll) 
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
save.image("Solution.RData")




start_time <- Sys.time()
res <- RcppDE::DEoptim(marginal.ll,lower=rep(0.001,n.par), 
                       upper=rep(100,n.par), 
                       DEoptim.control(NP = 10,strategy = 1, itermax = 100,
                                       CR = 0.5, F = 0.8, p=0.8, trace = TRUE,
                                       parallelType = 1))
end_time <- Sys.time()
end_time - start_time

save.image("Solution.RData")









start_time <- Sys.time()

res <- ga(type = "real-valued", 
                fitness =  marginal.ll,
                lower = rep(0.001,n.par), upper= rep(100,n.par),
                popSize = 10, maxiter = 5, pmutation = 0.5,
                optim = FALSE)
end_time <- Sys.time()
end_time - start_time
#Time difference of 43.66486 mins

save.image("Data\\AnalysisObjects.RData")

plot(res@fitness,type="l")

theta<-res@solution

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
res.X<-res.X[,-ncol(res.X)]
res.sum$Prob<-(res.sum$Sum/(res.sum$n*n.sim))
res.X <- full_join(res.sum,res.X,by="COD_OCUPACAO")
res.X <- res.X[,c(-2,-3)]

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
