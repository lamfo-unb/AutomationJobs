library(haven)
library(ggplot2)
library(tidyverse)

# jobzone_painel <- read_sas("C:/Users/b2657804/Documents/Meu Drive/LAMFO/Adicionar a automation/Data/jobzone_painel2.sas7bdat")
# jobzone_painel <- read_sas("C:/Users/rafal/Google Drive/LAMFO/Adicionar a automation/Data/jobzone_painel2.sas7bdat")
# write.csv2(jobzone_painel, 'Data/jobzone_painel.csv')
jobzone_painel <- read.csv2('Data/jobzone_painel.csv')

serie <- jobzone_painel %>% group_by(Job_Zone, ano) %>% 
  summarise(empr        = sum(empregados),
            rend        = sum(renda),
            rend_sm     = sum(renda_sm),
            rend_med    = sum(renda)/sum(empregados),
            rend_med_sm = sum(renda_sm)/sum(empregados)) %>% na.omit()


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
  scale_x_continuous(breaks = sort(unique(graph02.tbl$ano))[seq(1, 31, by = 3)]) +
  scale_colour_discrete(name="Automation",
                        breaks=c("1","2", "3", "4", "5"),
                        labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Cumulative Growth Rate")+
  theme(legend.position="bottom") 

#Smoothed time series
ggplot(graph01.tbl, aes(x=ano, y=cumulative, colour=as.factor(Job_Zone), group=as.factor(Job_Zone))) + 
  theme_bw() +
  scale_x_continuous(breaks = sort(unique(graph02.tbl$ano))[seq(1, 31, by = 3)]) +
  geom_smooth(method = 'loess', span = .3, se = FALSE) + 
  scale_colour_discrete(name="Automation",
                        breaks=c("1","2", "3", "4", "5"),
                        labels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5")) +  xlab("Year")+ ylab("Cumulative Growth Rate")+
  theme(legend.position="bottom") 

# Renda media
ggplot(serie, aes(ano, rend_med, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Média em Reais", color = "  Job\n Zone") +
  theme(legend.position="bottom") 

# Renda media em salarios minimos
ggplot(serie, aes(ano, rend_med_sm, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Média em Salários Mínimos", color = "  Job\n Zone") +
  theme(legend.position="bottom") 

# Renda Total
ggplot(serie, aes(ano, rend, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Total", color = "  Job\n Zone") +
  theme(legend.position="bottom") 


library(readxl)
GDP_GROWTH <- read_excel("Data/GDP_GROWTH.xls", skip = 3)
GDP_GROWTH <- GDP_GROWTH %>% filter(`Country Name` == "Brazil") %>% gather(year, growth) %>%
  filter(year %in% 1986:2016) %>% mutate(growth     = as.numeric(growth),
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


