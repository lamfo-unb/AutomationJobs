library(dplyr)

setwd("./Data/scrapy")

arquivos <- list.files(pattern='*.csv')
bases    <- lapply(arquivos, read.csv2, colClasses = "character" )

cbos <- bind_rows(bases)

#--------- Evitar duplicacao -------------------- 
scbos <- cbos[!duplicated(cbos$CBO2002), -c(1,3)]

cbo <- read.csv2("./Data/CBO94 - CBO2002 - Conversao com 90.csv", colClasses = c("character", "character") )

cbo <- cbo %>% left_join(scbos, by = c("CBO2002" = "CBO2002"))
#------------------------------------------------

write.csv2(cbo, row.names = F,
           file = "./Data/CBO2002_ISCO88.csv")

