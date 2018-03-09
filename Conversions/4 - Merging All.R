rm(list = ls())

library(dplyr)
CBO_OK   <- read.csv2("Conversions/CBO_OK.csv")
# CBO_ok_m1 <- read.csv2("Conversions/CBO_ok_M1.csv") 
# CBO_ok_m2 <- read.csv("Conversions/Manual_Check_completed.csv") %>% select(CBO2002, Job_Zone = X)

# CBO_OK <- bind_rows(CBO_OK, CBO_ok_m1) %>% distinct(CBO2002, Job_Zone)
# CBO_OK <- bind_rows(CBO_OK, CBO_ok_m2) %>% distinct(CBO2002, Job_Zone)

CBO_OK$Job_Zone <- as.character(CBO_OK$Job_Zone) 
CBO_OK$CBO2002 <- as.character(CBO_OK$CBO2002)
CBO_OK$CBO2002 <- sapply(CBO_OK$CBO2002, function(x){ifelse(nchar(x) < 6, paste0("0", x), x)})

rep <- CBO_OK %>% group_by(CBO2002) %>% summarise(rep = n())

CBO_OK <- CBO_OK %>% left_join(rep, by = "CBO2002")

write.csv2(CBO_OK, row.names = F, file = "Conversions/CBO_OK.csv")

# checking
length(unique(CBO_OK$CBO2002))


