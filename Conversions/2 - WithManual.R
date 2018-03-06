rm(list = ls())

library(dplyr)

Manual_Check <- read.csv("Conversions/Manual_Check.csv", colClasses = "character" )

# Manu_ok1 <- Manual_Check %>% filter(correct == 1) %>% distinct(CBO2002, Job_Zone)
# Manu_ok2 <- Manual_Check %>% filter(correct == 1) %>% distinct(CBO2002, Job_Zone)
# Manu_ok3 <- Manual_Check %>% filter(correct == 1) %>% distinct(CBO2002, Job_Zone)
# Manu_ok4 <- Manual_Check %>% filter(correct == 1) %>% distinct(CBO2002, Job_Zone)
# Manu_ok5 <- Manual_Check %>% filter(correct == 1) %>% distinct(CBO2002, Job_Zone)

Manu_ok = Manu_ok5


#--- Upadating Manual_Check ---#
Check <- Manual_Check %>% filter(!(CBO2002 %in% Manu_ok$CBO2002)) %>%
  distinct(CBO2002)

CBO_O <- read.csv2("Data/Occupation Structure/CBO2002 - Ocupacao.csv", colClasses = "character")
Manual_Check <- CBO_O %>% rename(CBO2002 = CODIGO) %>% filter(CBO2002 %in% Check$CBO2002)

write.csv2(Manual_Check, row.names = F, file = "Conversions/Manual_Check.csv")
