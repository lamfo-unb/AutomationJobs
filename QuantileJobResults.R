library(tidyverse)
prob<-read.csv("C:\\Users\\Pedro Albuquerque\\Dropbox\\Departamento de Administração\\Lamfo\\Automation\\Download\\ProbabilityBayes.csv")
serie<-read.csv("Download\\RAIS_CBO_1986-2016.csv")

median<-prob %>% 
        group_by(COD_OCUPACAO) %>% 
        summarise(Probability=median(Probability))

join <- median %>% 
        inner_join(serie,by=c("COD_OCUPACAO"="cbo2002"))