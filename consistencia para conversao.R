
library(dplyr)


transl <- read.csv2("./Data/translate_matching.csv", colClasses = "character" )
conver <- read.csv2("./Data/CBO2002_SOC.csv", colClasses = "character" )


check <- transl %>% rename(CBO2002 = CODIGO) %>% inner_join(conver, by = ("CBO2002"))

check_f <- check %>% group_by(CBO2002) %>% summarise(freq = n()) 

check_f %>% select(freq) %>% table(.)  

CBO_ok <- check_f %>% filter(freq == 1) %>% left_join(check, by = "CBO2002") %>% 
          group_by(CBO2002, code) %>% summarise(job_zone = Job_Zone.y)

CBO_miss <- check_f %>% filter(freq != 1) %>% left_join(check, by = "CBO2002") %>% 
            group_by(CBO2002, code) 

CBO_ok2 <- CBO_miss %>% filter(prox > 0.81) %>% ungroup() %>% group_by(CBO2002, code) %>% 
           summarise(job_zone = first(Job_Zone.x))

CBO_miss <- CBO_miss %>% filter(prox <= 0.81) 

length(unique(CBO_miss$CBO2002))

CBO_OK <- rbind(CBO_ok, CBO_ok2)

write.csv2(CBO_OK, row.names = F, file = "./Data/CBO_OK.csv")

# ==============================================================================

library(readxl)
Job_Zones <- read_excel("Data/All_Job_Zones.xls", skip = 3)

a1 <- Job_Zones %>% mutate(Code = substr(Code, 1, 6)) %>% group_by(Code, `Job Zone`) %>% summarise(freq = n())
a1 %>% ungroup() %>% group_by(Code) %>% summarise(freq = n()) %>% filter(freq == 1) %>% select(Code)


a2<- Job_Zones %>% mutate(Code = substr(Code, 1, 4)) %>% group_by(Code, `Job Zone`) %>% summarise(freq = n())
a2 %>% ungroup() %>% group_by(Code) %>% summarise(freq = n()) %>% filter(freq == 1) %>% select(Code)



CBO_miss

