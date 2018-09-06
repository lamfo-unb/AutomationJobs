library(ggplot2)
library(forecast)
library(tseries)
library(zoo)

serie<-read.csv("Download\\cbo1986_2016.csv")
cbo<-unique(serie$cbo2002)
c<-cbo[2]
k<-29 #Forecast 29 steps ahed
years<-data.frame(ano=seq(1986,2016))
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
  forecast<-forecast(autoArimaFit, h=((2016-max(time$ano))+k))
  df<-data.frame(ano=seq(max(time$ano)+1,2016+k),forecast=forecast$mean,serie="Previsão",li=forecast$lower[,2],ls=forecast$upper[,2])
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