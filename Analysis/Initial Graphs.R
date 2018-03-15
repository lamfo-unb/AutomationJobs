#Read the data
library(tidyverse)
library(ggplot2)
library(ineq)
library(PerformanceAnalytics)
base1<-read.csv2("Data/jobzone_painel.csv")

##############################################  Graph 1 #############################################################################

#Cumulative Growth Rate
graph01.tbl1 <- base1 %>%
  group_by(ano,Job_Zone) %>%
  summarise(empreg = sum(empregados))
  
graph01.tbl2 <- graph01.tbl1 %>%
  group_by(Job_Zone) %>%
  mutate(lag = lag(empreg)) %>%
  mutate(pct.change = (empreg - lag) / lag)

graph01.tbl2<-graph01.tbl2[!is.na(graph01.tbl2$pct.change),]

graph01.tbl <- graph01.tbl2 %>%
                group_by(Job_Zone) %>%
                mutate(cumulative= cumprod(1 + pct.change) - 1)

#pdf("GRAPH001.pdf")
p<-ggplot(graph01.tbl, aes( x=ano, y=cumulative, colour=as.factor(Job_Zone), group=as.factor(Job_Zone) )) + 
  geom_line() + scale_colour_discrete(name="Automation",
                                      breaks=c("1","2", "3", "4", "5"),
                                      labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Cumulative Growth Rate")

p<-p+theme_bw()
p
#dev.off()


##############################################  Graph 2 #############################################################################

graph02.tbl<- graph01.tbl %>% 
              group_by(Job_Zone) %>% 
              mutate(
                interpolated     = spline(x=ano, y=cumulative  , xout=ano)$y
              )

#pdf("GRAPH002.pdf")

p<-ggplot(graph02.tbl, aes( x=ano, y=interpolated, colour=as.factor(Job_Zone), group=as.factor(Job_Zone) )) + 
  geom_smooth(method = 'loess', span = .75, se = FALSE) + 
  scale_colour_discrete(name="Automation",
                                      breaks=c("1","2", "3", "4", "5"),
                                      labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Cumulative Growth Rate")
p<-p+theme_bw()
p
#dev.off()


