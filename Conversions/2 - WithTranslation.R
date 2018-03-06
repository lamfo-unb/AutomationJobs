rm(list = ls())

library(dplyr)
cbo_ok <- read.csv2("./Conversions/CBO_OK1.csv", colClasses = "character" )

CBO_O <- read.csv2("Data/Occupation Structure/CBO2002 - Ocupacao.csv", colClasses = "character")
CBO_O <- CBO_O %>% rename(CBO2002 = CODIGO) %>% filter(!(CBO2002 %in% cbo_ok$CBO2002))

CBO_S <- read.csv2("Data/Occupation Structure/CBO2002 - Sinonimo.csv", colClasses = "character")
CBO_S <- CBO_S %>% rename(CBO2002 = OCUPACAO) %>% filter(!(CBO2002 %in% cbo_ok$CBO2002))

check_cbo <- bind_rows(CBO_O, CBO_S)

write.csv2(check_cbo, row.names = F, file = "Conversions/CBO_Check.csv")

# ================================================================================= #
#--- After 'transl_en.py' ---# 
Trans_EN <- read.csv2("Conversions/trans_en.csv", colClasses = "character")
EN <- Trans_EN %>% filter(prox > 0.81) %>% distinct(CBO2002, Job_Zone) %>%
  group_by(CBO2002) %>% summarise(freq = n()) %>% filter(freq == 1) 

CBO_EN <- Trans_EN %>% filter(CBO2002 %in% EN$CBO2002, prox > 0.81) %>% 
  distinct(CBO2002, Job_Zone)

# ================================================================================= #

#--- Updating Check ---#
check_cbo2 <- check_cbo %>% filter(!(CBO2002 %in% CBO_EN$CBO2002))
write.csv2(check_cbo2, row.names = F, file = "Conversions/CBO_Check2.csv")

# ================================================================================= #
#--- After 'transl_pt.py' ---# 
Trans_PT <- read.csv2("Conversions/trans_pt.csv", colClasses = "character")
PT <- Trans_PT %>% filter(prox > 0.81) %>% distinct(CBO2002, Job_Zone) %>%
  group_by(CBO2002) %>% summarise(freq = n()) %>% filter(freq == 1) 

CBO_PT <- Trans_PT %>% filter(CBO2002 %in% PT$CBO2002, prox > 0.81) %>% 
  distinct(CBO2002, Job_Zone)

# ==========================================================================

CBO_Tranls <- bind_rows(CBO_EN, CBO_PT)
CBO_2002   <- bind_rows(cbo_ok, CBO_Tranls) %>% distinct(CBO2002, Job_Zone)

CBO_r <- read.csv2("Conversions/CBO_OK_r.csv", colClasses = "character")
CBO_r <- CBO_r %>% filter(!(CBO2002 %in% CBO_2002$CBO2002))

CBO_2002 <- bind_rows(CBO_2002, CBO_r) %>% distinct()

write.csv2(CBO_2002, row.names = F, file = "Conversions/CBO_OK.csv")

# ==========================================================================

Manual_Check <- bind_rows(Trans_EN, Trans_PT) %>% 
  left_join(check_cbo2, by = c("CBO2002", "TITULO")) %>%
  filter(CBO2002 != '', !(CBO2002 %in% CBO_2002$CBO2002)) %>% mutate(correct = '') %>%
  distinct("CBO2002", "Job_Zone", "prox", "TITULO", "trad", "title", "correct") %>%
  select("CBO2002", "Job_Zone", "prox", "TITULO", "trad", "title", "correct")

write.csv2(Manual_Check, row.names = F, file = "Conversions/Manual_Check.csv")

