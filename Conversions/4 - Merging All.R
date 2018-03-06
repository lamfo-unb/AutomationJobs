rm(list = ls())

library(dplyr)
CBO_OK   <- read.csv2("Conversions/CBO_OK.csv")
# CBO_ok_m1 <- read.csv2("Conversions/CBO_ok_M1.csv") 
# CBO_ok_m2 <- read.csv2("Conversions/CBO_ok_M2.csv") 

# CBO_OK <- bind_rows(CBO_OK, CBO_ok_m1) %>% distinct(CBO2002, Job_Zone)
# CBO_OK <- bind_rows(CBO_OK, CBO_ok_m2) %>% distinct(CBO2002, Job_Zone)
write.csv2(CBO_OK, row.names = F, file = "Conversions/CBO_OK.csv")

# checking
length(unique(CBO_OK$CBO2002))
