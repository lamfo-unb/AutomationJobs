library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(tidyverse)

last.rais<-2017
profile<-read.csv2("Data/Occupation Structure/CBO2002 - PerfilOcupacional.csv")
profile<-data.frame("cbo2002"=unique(profile[,5]))

serie<-read.csv("Download\\RAIS_CBO_1986-2017.csv")
names(serie)[2] <- "cbo2002"
#Remove strange CBO
serie<-inner_join(serie,profile, by="cbo2002")
cbo<-unique(serie$cbo2002)
c<-cbo[2]
k<-29 #Forecast 29 steps ahed
years<-data.frame(ano=seq(1986,last.rais))
conta<-1
for(c in cbo){
  time<-serie[which(serie$cbo2002==c),c("ano","empregados")]
  time <- merge(time,years,by="ano", all=T)
  time <- time[order(time$ano),]
  x <- zoo(time$empregados, order.by=time$ano)
  zs <- as.data.frame(na.approx(x))
  zs$ano=rownames(zs)
  time<-merge(time,zs,by="ano",all=T)
  time<-time[!is.na(time$`na.approx(x)`),]
  time$serie<-"Original"
  time$serie[is.na(time$empregados)]<-"Interpolado"
  time$empregados[is.na(time$empregados)]<-time$`na.approx(x)`[is.na(time$empregados)]
  time<-time[,-3]
  autoArimaFit <- auto.arima(time$empregados)
  forecast<-forecast(autoArimaFit, h=((last.rais-max(time$ano))+k))
  df<-data.frame(ano=seq(max(time$ano)+1,last.rais+k),forecast=forecast$mean,serie="Previs�o",li=forecast$lower[,2],ls=forecast$upper[,2])
  colnames(df)<-c("ano","empregados","serie","li","ls")
  time$li<-NA
  time$ls<-NA
  time<-rbind(time,df)
  time[time<0]<-0
  time$cbo2002<-c
  if(conta==1){
    finalForecast<-time
  }
  else{
    finalForecast<-rbind(finalForecast,time)
  }
  conta<-conta+1
}

write.csv(finalForecast,"Download\\Previsao.csv")
