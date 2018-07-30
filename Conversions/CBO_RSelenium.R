# Importando lista de CBOs
cbo <- read.csv2("./Data/CBO94 - CBO2002 - Conversao com 90.csv", colClasses = c("character", "character") )
cbo <- read.csv2("Data/Occupation Structure/CBO2002 - Ocupacao.csv", colClasses = c("character", "character") )

cbo <- data.frame(CBO2002 = unique(cbo$CODIGO))

cbo$cbo    <- NA
cbo$CIUo88 <- NA
cbo$titulo <- NA

#------------------ Para processamento em multiplas maquinas ------------------------------------#
cbo1 <- cbo[1:370,]
cbo2 <- cbo[371:740,]
cbo3 <- cbo[741:1100,]
cbo4 <- cbo[1101:1480,]
cbo5 <- cbo[1481:nrow(cbo),]
#------------------------------------------------------------------------------------------------#

# install.packages("RSelenium") 
library(RSelenium)

driver<- rsDriver()
remDr <- driver[["client"]]

page <- function(){
  remDr$navigate("http://www.mtecbo.gov.br/cbosite/pages/tabua/ConsultasConversao.jsf")
  
  optionelem1 <- remDr$findElement(using = "xpath", "//select[@id = 'formSite037:tabuaOrigem']/option[2]")
  optionelem1$clickElement()
  
  optionelem2 <- remDr$findElement(using = "xpath", "//select[@id = 'formSite037:tabuaDestino']/option[3]")
  optionelem2$clickElement()
}

page()
L <- 1
for (i in cbo$CBO2002) {
  print(L)
  print(round(L/nrow(cbo), 3))
  tryCatch(optionelem <- remDr$findElement(using = "xpath", "//*[@id='formSite037']/div[2]/input[1]"),
           error=function(e) page() )
  
  optionelem <- remDr$findElement(using = "xpath", "//*[@id='formSite037']/div[2]/input[1]")
  optionelem$sendKeysToElement(list(paste("", i), key = "enter"))
  
  webElems <- remDr$findElements(using = "xpath", "//*[@id='formSite037:objeto:tbody_element']/tr")
  resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))


  if(is.null(resHeaders)){
      cbo[L, 2:4] <- NA
      } else cbo[L, 2:4] <-  unlist(strsplit(resHeaders, "\n"))

  Sys.sleep(runif(1, 3.723254, 6.5132689))

  L <- L+1
}

remDr$close()
remDr$closeServer()

# write.csv2(cbo, row.names = F,
#            file = "./Data/scrapy/cbo_remoto.csv")
# 
# #====================================================================================#
# library(dplyr)
# 
# setwd("Data/scrapy")
# 
# arquivos <- list.files(pattern='*.csv')
# bases    <- lapply(arquivos, read.csv2, colClasses = "character" )
# 
# cbos <- bind_rows(bases)
# 
# #--------- Evitar duplicacao -------------------- 
# scbos <- cbos[!duplicated(cbos$CBO2002), -c(1,3)]
# 
# cbo <- read.csv2("./Data/CBO94 - CBO2002 - Conversao com 90.csv", colClasses = c("character", "character") )
# 
# cbo <- cbo %>% left_join(scbos, by = c("CBO2002" = "CBO2002"))
# #------------------------------------------------

write.csv2(cbo, row.names = F,
           file = "./Data/CBO2002_ISCO88.csv")

