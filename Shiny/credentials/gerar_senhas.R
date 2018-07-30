## Gerar Senhas ###
library(dplyr)
library(purrr)
library(digest)
library(stringr)

emails <- readRDS('~/AutomationJobs/R_scrap_selenium/emails_filtro2.rds')

teste2 <- emails %>%
          mutate( Nome_Espaco = map( Nomes , str_split , " "),
                  Login_Senha = map( Nome_Espaco ,
                                     ~  paste( str_sub( .x[[1]] , 1 , 2 ) , collapse = "" ) %>% 
                                        iconv(to = "ASCII//TRANSLIT"))) 
                              
credentials2 <- data.frame(Nome = teste2[["Nomes"]] ,
                           user = teste2[["Login_Senha"]] %>% unlist(),
                           pw   = map_chr( teste2[["Login_Senha"]] , digest ))

credentials2 <- rbind(credentials2 , 
                      data.frame(Nome = c('LAMFO','Nubank'),
                                 user = c('OFMAL','knabuN'),
                                 pw   = c(digest("OFMAL"),
                                          digest("knabuN") )) )

credentials2 <- credentials2 %>% mutate(locked_out = FALSE)

saveRDS(credentials2,'~/AutomationJobs/Shiny/credentials/credentials2.rds')
