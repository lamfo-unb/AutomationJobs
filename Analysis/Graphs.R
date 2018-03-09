
library(haven)
jobzone_painel <- read_sas("~/Meu Drive/LAMFO/Adicionar a automation/Data/jobzone_painel2.sas7bdat")

write.csv2(jobzone_painel, 'Data/jobzone_painel.csv')



library(dplyr)
serie <- jobzone_painel %>% group_by(Job_Zone, ano) %>% 
  summarise(empr        = sum(empregados),
            rend        = sum(renda),
            rend_sm     = sum(renda_sm),
            rend_med    = sum(renda)/sum(empregados),
            rend_med_sm = sum(renda_sm)/sum(empregados))



library(ggplot2)
# Renda media
ggplot(serie, aes(ano, rend_med, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1)

# Renda media em salarios minimos
ggplot(serie, aes(ano, rend_med_sm, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1) + theme_bw() + geom_point() +
  scale_x_continuous(breaks = sort(unique(serie$ano))[seq(1, 31, by = 3)]) +
  labs(x = "Ano", y = "Renda Média em Salários Mínimos", color = "  Job\n Zone") +
  theme(legend.position="bottom") 


# Numero de Empregados
ggplot(serie, aes(ano, empr, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1)

# Renda Total
ggplot(serie, aes(ano, rend, group = Job_Zone, colour = factor(Job_Zone))) + 
  geom_line(lwd=1)