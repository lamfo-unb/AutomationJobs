library(tidyverse)
library(kergp)
library(earth)
library(RcppEigen)
library(RcppParallel)
library(MASS)
library(tidyverse)
library(gpuR)
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

#List all context GPU
listContexts()

# set second context
setContext(1L)
currentDevice()

#Check if double is avaible - if FALSE must define type='float'
deviceHasDouble()

n.par<-ncol(X)+2
theta<-rep(50,n.par)
#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
  #Parameters
  sigma2f <- theta[1]
  sigma.n <- theta[2]
  lambda  <- theta[3:length(theta)]
  n <- nrow(X)
  Y.gpu <- vclVector(Y, type = "float")  
  #GPU
  ### start_time <- Sys.time()
  #lambdaMat <- vclMatrix(diag(1/lambda),type='float')
  lambdaMat <- vclMatrix(diag(1/lambda))
  #X.gpu <- vclMatrix(X, type='float')
  X.gpu <- vclMatrix(X)
  X.gpu <- X.gpu %*% lambdaMat
  k.xx <- suppressWarnings(gpuR::distance(X.gpu,X.gpu, method="sqEuclidean"))
  k.xx <- sigma2f*exp(-0.5*k.xx)
  k.xx <- k.xx + sigma.n^2*diag(1, n)
  L <-  chol(k.xx)
  logdet <- 2*sum(log(as.matrix(diag(L))))
  
  k1<-k.xx%*%Y.gpu
  k1<-as.numeric(as.matrix(k1%*%Y.gpu))
  #Information
  inf <- as.numeric((-0.5)*(k1))
  
  #Regularization
  det <- gpuR::det(k.xx) 
  
  ### end_time <- Sys.time()
  ### end_time - start_time
  if(is.infinite(logdet) | is.na(logdet)){
    return(+1e+10)
  }
  else{
    reg <- as.numeric((-0.5)*logdet)
    #Normalization
    nor <- -(n/2)*log(2*pi)
  }
  #Return
  return(-(inf+reg+nor))
}


# Calculate the number of cores
no_cores <- detectCores() 

# Initiate cluster
cl <- makeCluster(no_cores)

start_time <- Sys.time()
DEctrl <- DEoptim.control(NP = 10,strategy = 1, itermax = 6,
                          CR = 0.5, F = 0.8, p=0.8, trace = TRUE,
                          parallelType = 1,packages=c("gpuR"),parVar=c("X","Y"))
res <- DEoptim(marginal.ll,lower=rep(0.001,n.par), 
               upper=rep(100,n.par), DEctrl)
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)

theta<-res$optim$bestmem
plot(res$member$bestvalit)
save.image("Solution.RData")



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
