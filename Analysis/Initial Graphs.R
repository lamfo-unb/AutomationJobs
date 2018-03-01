#Read the data
library(tidyverse)
library(ggplot2)
library(ineq)
base1<-read.csv("Data\\automation_top1000.csv")

##############################################  Gráfico 1: #############################################################################

graph01.tbl1 <- base1 %>% 
               group_by(ano) %>% 
               summarise(total=sum(empregados))

graph01.tbl2 <- base1 %>% group_by(ano,Job_Zone) %>% 
               summarise(numerator=sum(empregados)) 
              
  
graph01.tbl <- graph01.tbl1 %>% full_join(graph01.tbl2,by="ano") %>% 
               mutate(freq=numerator/total,
                      JobZone=as.character(Job_Zone)) 
               
#pdf("GRAPH001.pdf")
p<-ggplot(graph01.tbl, aes( x=ano, y=freq, colour=as.factor(JobZone), group=as.factor(JobZone) )) + 
  geom_line() + scale_colour_discrete(name="Automation",
                                    breaks=c("2", "3", "4", "5"),
                                    labels=c("Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Frequency")

p<-p+theme_bw()
p
#dev.off()


##############################################  Gráfico 2: #############################################################################

graph02.tbl1 <- base1 %>% 
  group_by(ano) %>% 
  summarise(total=sum(renda))

graph02.tbl2 <- base1 %>% group_by(ano,Job_Zone) %>% 
  summarise(numerator=sum(renda)) 


graph02.tbl <- graph02.tbl1 %>% full_join(graph02.tbl2,by="ano") %>% 
  mutate(freq=numerator/total,
         JobZone=as.character(Job_Zone)) 

#pdf("GRAPH002.pdf")
p<-ggplot(graph02.tbl, aes( x=ano, y=freq, colour=as.factor(JobZone), group=as.factor(JobZone) )) + 
  geom_line() + scale_colour_discrete(name="Automation",
                                      breaks=c("2", "3", "4", "5"),
                                      labels=c("Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Income")

p<-p+theme_bw()
p
#dev.off()

##############################################  Gráfico 3: #############################################################################

graph03.tbl <- base1 %>% group_by(ano,Job_Zone) %>% 
               summarise(gini.idx=ineq(renda)) %>% 
               mutate(JobZone=as.character(Job_Zone)) 


#pdf("GRAPH003.pdf")
p<-ggplot(graph03.tbl, aes( x=ano, y=gini.idx, colour=as.factor(JobZone), group=as.factor(JobZone) )) + 
  geom_line() + scale_colour_discrete(name="Automation",
                                      breaks=c("2", "3", "4", "5"),
                                      labels=c("Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Gini Index")

p<-p+theme_bw()
p
#dev.off()