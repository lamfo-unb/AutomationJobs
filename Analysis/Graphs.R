library(ggplot2)
library(tidyverse)

serie <- read.csv2('Data/jobzone_painel.csv')

# Séries Temporais --------------------------------------------------------

# Number of employees
ggplot(serie, aes(ano, empr, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Número de Empregados", color = "  Job\n Zone") +
  theme(legend.position="bottom") 

# Number of employees (Cumulative Growth Rate)
graph01.tbl1 <- serie %>%
  group_by(ano,Job_Zone) %>%
  summarise(empreg = sum(empr))

graph01.tbl2 <- graph01.tbl1 %>%
  group_by(Job_Zone) %>%
  mutate(lag = lag(empreg)) %>%
  mutate(pct.change = (empreg - lag) / lag)

graph01.tbl2<-graph01.tbl2[!is.na(graph01.tbl2$pct.change),]

graph01.tbl <- graph01.tbl2 %>%
  group_by(Job_Zone) %>%
  mutate(cumulative= cumprod(1 + pct.change) - 1)

#Raw time series
ggplot(graph01.tbl, aes(x=ano, y=cumulative, colour=as.factor(Job_Zone), group=as.factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(graph01.tbl$ano))[seq(1, 31, by = 3)]) +
  scale_colour_discrete(name="Automation",
                        breaks=c("1","2", "3", "4", "5"),
                        labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Cumulative Growth Rate")+
  theme(legend.position="bottom") 

library(readxl)
GDP_GROWTH <- read_excel("Data/GDP_GROWTH.xls", skip = 3)
GDP_GROWTH <- GDP_GROWTH %>% 
  gather(year, growth) %>%
  filter(year %in% 1986:2017) %>% 
  mutate(growth     = as.numeric(growth),
         cumulative = (cumprod(growth/100 + 1) - 1))

ggplot(GDP_GROWTH, aes(year, growth, group = 1)) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_discrete(breaks = sort(unique(GDP_GROWTH$year))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Total", color = "  Job\n Zone") +
  theme(legend.position="bottom") 

# ============================================================================== #
# ========================= Cumulative GDP Growth Rate ========================= #
# ============================================================================== #
ggplot(GDP_GROWTH, aes(year, cumulative, group = 1)) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_discrete(breaks = sort(unique(GDP_GROWTH$year))[seq(1, 31, by = 3)]) +
  labs(x = "Year", y = "Cumulative Growth Rate") +
  theme(legend.position="bottom") 




#Add GDP and Growth rate
graph03.tbl<-graph01.tbl %>% 
  select(ano,Job_Zone, cumulative) %>% 
  rename(year = ano)
GDP_GROWTH2<-GDP_GROWTH %>% 
  mutate(Job_Zone="GDP") %>% 
  select(-growth)
GDP_GROWTH2$year<-as.numeric(GDP_GROWTH2$year)
graph03.tbl$Job_Zone<-as.character(graph03.tbl$Job_Zone)

final<-bind_rows(graph03.tbl,GDP_GROWTH2)

#Colors
mycolors <- RColorBrewer::brewer.pal(8, "YlOrRd")
mycolors<-mycolors[c(-1,-2,-3)]
mycolors<-c(mycolors,"#000000")

#Negative GDP
GDP_GROWTH$year[which(GDP_GROWTH$growth<0)]

#Types of data
final$Type<-ifelse(final$Job_Zone=="GDP","Cumulative GDP","Job Zones")

ggplot(final, aes(x=year, y=cumulative, colour=as.factor(Job_Zone), group=as.factor(Job_Zone))) + 
  geom_rect(aes(xmin = 2014, xmax = 2016, 
                ymin = -Inf, ymax = Inf), alpha = 0.01) + 
  geom_line(aes(linetype=Type),lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(final$year))[seq(1, 31, by = 3)]) +
  xlab("Year")+ ylab("Cumulative Growth Rate")+
  scale_colour_manual(name="Automation",
                      breaks=c("1","2", "3", "4", "5","GDP"),
                      labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5","GDP"),
                      values = mycolors)+
  theme(legend.position="bottom")


#Correct GDP
final$cumulative[which(final$year<=1987 & final$Type=="Cumulative GDP")]<-0
final<-subset(final,year>1986)

#Colors
mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")
mycolors<-c(mycolors,"#000000")

ggplot(final, aes(x=year, y=cumulative, colour=as.factor(Job_Zone), group=as.factor(Job_Zone))) + 
  geom_line(aes(linetype=Type),lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(final$year))[seq(1, 31, by = 3)]) +
  xlab("Year")+ ylab("Cumulative Growth Rate")+
  scale_colour_manual(name="Automation",
                      breaks=c("1","2", "3", "4", "5","GDP"),
                      labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5","GDP"),
                      values = mycolors)+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical")
ggsave('serie1.pdf', units="in", width=5, height=5)

#Fall percentage from \\ 2014 to 2016 in the CGR 
fall2014<-subset(final, year==2014)
fall2016<-subset(final, year==2016)
round((fall2014$cumulative-fall2016$cumulative)*100,4)

