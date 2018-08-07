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

codifica <- function(x){
  cod <- strsplit(x, split = " ")
  cod <- paste0(sapply(cod, function(x) substr(x,1,2)), collapse = "")
  return(cod)
}

names$Nome <- ifelse(names$Nome == "Lucia Specia", "Lucia Specia A",names$Nome)

names <- names %>% 
  group_by(Nome) %>% 
  mutate(cod = codifica(Nome)) %>% 
  distinct()

auto <- left_join(auto, names, by = c("Usuario" = "cod"))


# Areas Aplicadas ---------------------------------------------------------
area <- function(x){
  b = regexpr("Doutorado em Doutorado em |Doutorado em |Doutorado pel", x)
  e = regexpr(" pela| pelo| - UNI", x)
  area = substr(x, attr(b, "match.length") + 1, e - 1)
  return(area)
}

# science <- fields
# science[science %in% c("Ciência da Computação", "Computação Aplicada", "Desenho Industrial",
#                        "Engenharia", "Informática", "Modelagem Computacional")] <- "Applied sciences"
# science[science %in% c("Ciências Farmacêuticas", "Ciências Fisiológicas", "Ciências Médicas")] <-  "Healthcare"
# science[science %in% c("Cognitive Science", "Photogrammetry and Remote Sensing")] <- "Interdisciplinary"
# science[science %in% c("Química", "Tecnologia de Processos Químicos e Bioquímicos", "Tecnologia Nuclear")] <- "Chemistry"
# science[science %in% c("Geologia")] <- "Earth science"

science <- fields
science[science %in% c("Ciência da Computação", "Computação Aplicada", "Desenho Industrial",
                       "Engenharia", "Informática", "Modelagem Computacional", 
                       "Ciências Farmacêuticas", "Ciências Fisiológicas", "Ciências Médicas")] <- "Applied sciences"
science[science %in% c("Cognitive Science", "Photogrammetry and Remote Sensing")] <- "Interdisciplinary"
science[science %in% c("Química", "Tecnologia de Processos Químicos e Bioquímicos", "Tecnologia Nuclear",
                       "Geologia", "Física")] <- "Physical"


science_tab <- data.frame(Science = science)
science_tab <- science_tab %>% group_by(Science) %>% summarise(freq = n()) %>% print(n=50)


auto <- auto %>% mutate(science = area(Doutorado)) 

# Branch of Science
auto %>% mutate(
  science = case_when(
    science %in% c(unique(grep("Computa|Engineering", science, value = T)),
                   "Desenho Industrial", "Doctorat En Informatique", "Frontier Informatics",
                   unique(grep("Engenharia|Engineering", science, value = T)), "Informática", 
                   "Ciências Farmacêuticas", "Ciências Fisiológicas",
                   unique(grep("Médica", science, value = T))) ~ "Applied sciences",
    science %in% c("Cognitive Science", "Photogrammetry and Remote Sensing") ~ "Interdisciplinary",
    science %in% c(unique(grep("Química", science, value = T)), "Tecnologia Nuclear", "Geologia",
                   "Tecnologia de Processos Químicos e Bioquímicos", "Física") ~ "Physical sciences",
    TRUE ~ "Missing"
  )) %>% 
  group_by(science) %>% summarise(Answers = n(),
                                  Researchers = n_distinct(Usuario),
                                  Mean_Answer = n()/ n_distinct(Usuario))

