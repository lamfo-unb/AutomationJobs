library(haven)
library(ggplot2)
library(dplyr)

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

# Numero de Empregados
ggplot(serie, aes(ano, empr, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Número de Empregados", color = "  Job\n Zone") +
  theme(legend.position="bottom") 

# Renda Total
ggplot(serie, aes(ano, rend, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Total", color = "  Job\n Zone") +
  theme(legend.position="bottom") 


