library(tidyverse)
prob<-read.csv("C:\\Users\\Pedro Albuquerque\\Dropbox\\Departamento de Administração\\Lamfo\\Automation\\Download\\ProbabilityBayes.csv")
serie<-read.csv("Download\\RAIS_CBO_1986-2016.csv")

median<-prob %>% 
        group_by(COD_OCUPACAO) %>% 
        summarise(Probability=median(Probability))

join <- median %>% 
        inner_join(serie,by=c("COD_OCUPACAO"="cbo2002"))


join$dummy<-ifelse(join$Probability>0.7537,1,0)
total<-join %>% group_by(ano) %>% summarise(total=sum(dummy*empregados))
tail(total)

total<-join %>% group_by(ano) %>% summarise(total=sum(empregados))
tail(total)


quantile<-quantile(median$Probability)

join$quantile<-cut(join$Probability,breaks=quantile,labels=c("Q1","Q2","Q3","Q4"))

sumJobs<- join %>% 
          group_by(quantile,ano) %>% 
          summarise(emp=sum(empregados)) %>% 
          filter(ano %in% c(2014,2015,2016))