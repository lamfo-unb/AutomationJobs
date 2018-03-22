#cbo<-read.csv2("CBO2002.csv")
#http://www.mtecbo.gov.br/cbosite/pages/downloads.jsf

library(dplyr)
lista<- cbo %>% 
  na.omit() %>%
  group_by(COD_GRANDE_GRUPO,COD_SUBGRUPO_PRINCIPAL,COD_SUBGRUPO,COD_FAMILIA,COD_OCUPACAO,NOME_GRANDE_AREA) %>% 
  mutate(Texto = paste0(COD_ATIVIDADE,"-",NOME_ATIVIDADE, collapse = "\n")) %>% 
  select(-COD_ATIVIDADE,-NOME_ATIVIDADE,-SGL_GRANDE_AREA) %>% 
  unique()
lista$NOME_GRANDE_AREA<-as.character(lista$NOME_GRANDE_AREA)
saveRDS(lista,"CBO.RDS")