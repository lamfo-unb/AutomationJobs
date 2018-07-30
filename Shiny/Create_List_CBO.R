#cbo<-read.csv2("CBO2002.csv")
#http://www.mtecbo.gov.br/cbosite/pages/downloads.jsf


cbo <- read.csv2("Data/Occupation Structure/CBO2002 - PerfilOcupacional.csv")
ocu <- read.csv2("Data/Occupation Structure/CBO2002 - Ocupacao.csv")

library(dplyr)
lista <- left_join(cbo, ocu, by = c("COD_OCUPACAO" = "CODIGO")) %>% 
  na.omit() %>% 
  group_by(TITULO, COD_OCUPACAO, NOME_GRANDE_AREA) %>% 
  mutate(Texto = paste0(COD_ATIVIDADE, "-", NOME_ATIVIDADE, collapse = "\n") ) %>% 
  select(TITULO, COD_OCUPACAO, NOME_GRANDE_AREA, Texto) %>% 
  unique() %>% ungroup() %>%
  group_by(TITULO, COD_OCUPACAO) %>% 
  mutate(Texto = paste0(NOME_GRANDE_AREA, "\t", Texto, collapse = "\v") ) %>% 
  select(TITULO, COD_OCUPACAO, Texto) %>%
  unique()
  
lista$TITULO <- as.character(lista$TITULO)
saveRDS(lista,"CBO.RDS")
