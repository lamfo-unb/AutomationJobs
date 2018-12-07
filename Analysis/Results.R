#Remove LS
rm(list=ls())
library(tidyverse)
library(scales)
library(pdftools)
profile<-read.csv2("Data/Occupation Structure/CBO2002 - PerfilOcupacional.csv")
profile<-data.frame("cbo2002"=unique(profile[,5]))

#Read the results
results<-readRDS("Analysis/ProbabilityGP_round.rds")
forecast<-read.csv("Download\\Previsao.csv")


#Remove strange CBO
serie<-inner_join(forecast,profile, by="cbo2002")

#Calculate the total
total.cbo <- forecast %>% 
              mutate(cbo2002 = factor(cbo2002)) %>% 
              group_by(cbo2002, ano) %>% 
              summarise(Total=sum(empregados))
length(unique(forecast$cbo2002))
#Calculate the median
median.cbo <- results %>% 
              group_by(COD_OCUPACAO) %>% 
              summarise(Probability=median(Probability))
colnames(median.cbo)<-c("cbo2002","Probability")
length(unique(results$COD_OCUPACAO))
#Merge data
final <- inner_join(total.cbo, median.cbo, by = "cbo2002" )

#Create the classes
quantile(final$Probability)
final$Class<-cut(final$Probability,breaks = c(0, 0.25, 0.5, 0.75, 1),
                 labels = c("Very Low","Low","High", "Very High"))
final <- final %>% 
  group_by(Class, ano) %>% 
  summarise(Total=sum(Total))

table(final$Class)

#Colors
mycolors <- RColorBrewer::brewer.pal(4, "Pastel1")

ggplot(final, aes(x=ano, y=Total, colour=as.factor(Class), group=as.factor(Class))) + 
  geom_line(aes(),lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(final$ano))[seq(1, length(unique(final$ano)), by = 10)]) +
  xlab("Year")+ ylab("Number of employees")+
  scale_colour_manual(name="Level of Automation", values = mycolors)+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical")+
  scale_y_continuous(labels = comma)+guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave('serie2.pdf', units="in", width=7, height=5)

