library(readxl)
library(dplyr)
cbo2002_isco88 <- read.csv2("./Data/CBO2002_ISCO88.csv",
                            colClasses = "character" ) %>% distinct(CBO2002, CIUO88)

isco88_isco08 <- read_excel("./Data/ISCO88_ISCO08.xlsx", col_types = "text")

isco08_soc <- read_excel("./Data/ISCO08_SOC.xls", skip = 6, col_types = "text")

Job_Zones <- read_excel("./Data/All_Job_Zones.xls", skip = 3, col_types = "text")

#---------------------------------------------------------------------------------------------------------
auto <- Job_Zones %>%  mutate(Code = substr(Code,1,7)) %>% 
        rename(Job_Zone = `Job Zone`) %>% distinct(Job_Zone, Code, Occupation)
#---------------------------------------------------------------------------------------------------------

cbo2002_soc <- cbo2002_isco88 %>% left_join(isco88_isco08, by = c("CIUO88" = "ISCO-88 code")) %>%
               distinct(CBO2002, `ISCO 08 Code`)
cbo2002_soc <- cbo2002_soc %>% left_join(isco08_soc, by = c("ISCO 08 Code" = "ISCO-08 Code")) %>%
               distinct(CBO2002, `2010 SOC Code`)
cbo2002_soc <- cbo2002_soc %>% left_join(auto, by = c("2010 SOC Code" = "Code"))

cbo2002_soc <- cbo2002_soc %>% distinct(CBO2002, Job_Zone) %>% na.omit()

# Check matching
check <- cbo2002_soc %>% group_by(CBO2002) %>% summarise(count = n())
table(check$count)

check <- check %>% filter(count != 1)

cbo_ok <- cbo2002_soc %>% filter(!(CBO2002 %in% check$CBO2002))
# ---

write.csv2(cbo2002_soc, row.names = F, file = "Conversions/CBO_OK_r.csv")
write.csv2(cbo_ok, row.names = F, file = "Conversions/CBO_OK1.csv")
