library(tidyverse)
library(MASS)
library(parallelDist)
library(doParallel)
library(foreach)
library(gpuR)
library(pbapply)
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

n.par<-3
theta<-rep(1,n.par)
#Marginal Log-Likelihood (Type-II Likelihood)
marginal.ll<-function(theta){
    #Parameters
    sigma2f <- theta[1]
    sigma.n <- theta[2]
    lambda  <- theta[3]
    n <- nrow(X)
    Y.gpu <- vclVector(Y)  

    #X.gpu <- vclMatrix(X, type='float')
    X.g <- X/lambda
    X.gpu <- vclMatrix(X.g)
    k.xx <- suppressWarnings(gpuR::dist(X.gpu,method="sqEuclidean"))
    k.xx <- sigma2f*exp(-0.5*k.xx)
    k.xx <- k.xx + sigma.n^2*diag(1, n)
    L <-  chol(k.xx)
    logdet <- 2*sum(log(as.matrix(diag(L))))
    #Regularization
    invL <- solve(L)
    invK <- t(invL)%*%invL        
    #Information
    
    
    k1<-k.xx%*%Y.gpu
    k1<-as.numeric(as.matrix(k1%*%Y.gpu))
    #Information
    inf <- as.numeric((-0.5)*(k1))
    if(is.infinite(logdet) | is.na(logdet) | is.nan(logdet)){
      return(-Inf)
    }
    else{
      reg <- as.numeric((-0.5)*logdet)
      #Normalization
      nor <- -(n/2)*log(2*pi)
    }
    #Return Log-likelihood
    return(inf+reg+nor)
}
#gri<-50
n.par<-3
#initialPop<-eval(seq(0.001,100,length.out = gri))
initialPop<-2^seq(-10,10)
final <- vector("list", n.par) 
for(p in 1:n.par){
  final[[p]]<-initialPop
}
#hours <- (2.5*(gri^n.par))/1000
initialPop<-do.call(expand.grid, final)
initialPop<-as.data.frame(initialPop)

#Check if double is avaible - if FALSE must define type='float'
deviceHasDouble()

#List all context GPU
listContexts()
currentContext()
currentDevice() 

# set second context
setContext(1L)

# Calculate the number of cores
#no_cores <- 12
no_cores <- 2

# Initiate cluster
cl <- makeCluster(no_cores)
#clusterExport(cl=cl,varlist=c("X","Y"))

#Registra os clusters a serem utilizados
registerDoParallel(cl)


start_time <- Sys.time()
start_time
#Inicio 9:40 28/9 - Expectativa 12/11

res <- foreach(i=1:nrow(initialPop), .combine=c, .packages = "gpuR", .export = c("X","Y"), .errorhandling = 'pass') %dopar% {
  theta<-as.numeric(initialPop[i,])
  res<-marginal.ll(theta)
  res
}


end_time <- Sys.time()
end_time - start_time
stopCluster(cl)

maxInd<-0
maxValue<- -1e+20
for(i in 1:length(res)){
  if(length(res[[i]])>0){
    if(res[[i]]>maxValue){
      maxInd<-i
      maxValue<-res[[i]]
    }
  }
}

theta<-initialPop[which(max(res)==res),]
save.image("Solution.RData")


#Parameters            
sigma2f <- theta[1]
sigma.n <- theta[2]
lambda  <- rep(theta[3],ncol=ncol(X))

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