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
library(bayesm)
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

#Bayesian Regression
R<-6000
set.seed(2545)


Data1<-list(y=Y,X=X); Mcmc1=list(R=R) 
out<-runiregGibbs(Data=Data1,Mcmc=Mcmc1)
proba<-rep(NA,R*nrow(X.star))
for(i in 1:nrow(X.star)){
  beta<-out$betadraw[1001:R,]
 res<- as.numeric(beta%*%X.star[i,])
 res<- res + mean(df$Score)
 res<- exp(res)/(1+exp(res))
 if(i==1){
   final<-data.frame(COD_OCUPACAO=df.star[i,"COD_OCUPACAO"],Probability=res)
 }
 else{
   temp<-data.frame(COD_OCUPACAO=df.star[i,"COD_OCUPACAO"],Probability=res)
   final<-rbind(final,temp)
 }
}

final <- final[order(final$COD_OCUPACAO),] 
write.csv(final,"ProbabilityBayes.csv")
saveRDS(final, "ProbabilityBayes.rds")

