rm(list = ls())

library(readr)
library(tidyverse)

auto <- read_csv("Data/Automation_sheet.csv")
glimpse(auto)
names <- read_csv("R_scrap_selenium/planilha_nomes.csv")
glimpse(names)

auto_d <- auto %>% 
  group_by(Usuario) %>% 
  summarise(Freq = n(),
            Prob_m = mean(Prob),
            Prob_min = min(Prob),
            Prob_max = max(Prob))

hist(auto_d$Freq, 50)
summary(auto_d$Freq)                                     


cbo <- auto %>% 
  group_by(CBO_5D) %>%  
  summarise(Freq = n(),
            Prob_m = mean(Prob),
            Prob_min = min(Prob),
            Prob_max = max(Prob))

table(cbo$Freq) 
mean(cbo$Freq)

names$Doutorado[names$Nome=="Francisco Quevedo Camargo"] <- "Doutorado em Biologia pela Universidade de Oxford"
names$Nome <- ifelse(names$Nome == "Lucia Specia", "Lucia Specia A",names$Nome)


names <- names %>%
  mutate( Nome_Espaco = map( Nome , str_split , " "),
          Login_Senha = unlist(map( Nome_Espaco , ~  paste( str_sub( .x[[1]] , 1 , 2 ) , collapse = "" ) ))) 

names <- names %>% 
  mutate(Login_Senha = iconv(Login_Senha, from="UTF-8",to = "ASCII//TRANSLIT"))


auto <- left_join(auto, names, by = c("Usuario" = "Login_Senha"))

# Areas Aplicadas ---------------------------------------------------------
area <- function(x){
  b = regexpr("Doutorado em Doutorado em |Doutorado em |Doutorado pel", x)
  e = regexpr(" pela| pelo| - UNI", x)
  area = substr(x, attr(b, "match.length") + 1, e - 1)
  return(area)
}


auto <- auto %>% mutate(science = area(Doutorado)) 

# Branch of Science
auto %>% mutate(
  science = case_when(
    science %in% c(unique(grep("Computa|Engineering", science, value = T)),
                   "Desenho Industrial", "Doctorat En Informatique", "Frontier Informatics",
                   unique(grep("Engenharia|Engineering", science, value = T)), "Informática", 
                   "Ciências Farmacêuticas", "Ciências Fisiológicas", "Saúde Coletiva", "Biologia",
                   unique(grep("Médica", science, value = T))) ~ "Applied sciences",
    science %in% c("Cognitive Science", "Photogrammetry and Remote Sensing") ~ "Interdisciplinary",
    science %in% c(unique(grep("Química", science, value = T)), "Tecnologia Nuclear", "Geologia",
                   "Tecnologia de Processos Químicos e Bioquímicos", "Física") ~ "Physical sciences",
    TRUE ~ "Missing"
  )) %>% 
  group_by(science) %>% summarise(Answers = n(),
                                  Researchers = n_distinct(Usuario),
                                  Mean_Answer = n()/ n_distinct(Usuario))


auto %>% filter(is.na(Doutorado)) %>% group_by(Usuario, Nome) %>% summarise(freq=n())
