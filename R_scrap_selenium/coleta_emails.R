
library(RSelenium)
library(gsheet)
#library(URLencode)
#library(rvest)
library(tidyverse)
library(dplyr)
library(stringr)


nomes<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1Isv2abfKPo__Hxxi0CpGQ6pIjS-mbzUmmHTBorA9HRY/edit?ouid=117131839543201915532&usp=sheets_home&ths=true')
nomes<-nomes[is.na(nomes$`E-mail`),]
regex<-"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"

#temp<-data.frame("Nomes"=nomes$Nome[1] , "Email" = 'exemplo@abc.com' )
temp <- readRDS('emails_temporarios.rds')

# fprof <- makeFirefoxProfile(list(
#   "network.proxy.socks" = "squid.home-server"
#   , "network.proxy.socks_port" = 3128L
#   , "network.proxy.type" = 1L
# )
# )

#remDr <- remoteDriver(browser = 'chrome')
# remDr <- remoteDriver(remoteServerAddr = "localhost", 
#                       port = 4444L, browserName = "firefox",
#                       extraCapabilities = fprof)

 
# driver<- rsDriver(port = 4444L , 
#                   browser = 'firefox',
#                   extraCapabilities = fprof)

# Abre e fecha esse navegador

driver <- rsDriver()
remDr <- driver[["client"]]

# abre o navegador
remDr$open()

length(table(temp$Nomes))
# i = 1452
# 675 892 926 985 1234 1341 1390 nao pegou
# 945 = leb@pea.usp.br
for( i in 1452:nrow(nomes) ){
  
  
  remDr$navigate("http://www.google.com/")
  Sys.sleep(sample(seq(2,8,length.out = 20),1))

  pesquisar <- paste0('"',nomes$Nome[i],'"',' e-mail ') 
  
  webElem <- remDr$findElement(using = "css", 
                               "[name = 'q']")
  
  webElem$sendKeysToElement(list(pesquisar))
  
  webElem <- remDr$findElement(using = "css", 
                               "#tsf > div.tsf-p > div.jsb > center > input[type=\"submit\"]:nth-child(1)") 
  Sys.sleep(sample(seq(2,9,length.out = 30),1 ))
  webElem$clickElement()

     
  webElems <- remDr$findElements(using = 'css selector', 
                                 "div.rc")
  # Primeira Pagina
  
  resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
  
  emails <- str_match( resHeaders , "\\S*@\\S*(\\.)?") [,1] 
  
  temp1 <- data.frame("Nomes" = nomes$Nome[i] , 
                      "Email" = emails )
  
  temp<-rbind(temp,temp1)
  
  Sys.sleep(sample(seq(2,7,length.out = 40),1 ))
  gc()
  Sys.sleep(3)
   # Segunda Pagina
 
    Sys.sleep(sample(seq(1.5,3,length.out = 10),1))
    css2 <- '#pnnext > span:nth-child(2)'
 
 
      webProximo <- tryCatch({
          suppressMessages({
           remDr$findElement(using = 'css selector' ,  css2)
          })
      },
        error = function(e) {
          NA_character_
        }
      )
 
    if(is.na(webProximo)) { next }
 
    ## Pegar textos ...
 
    webElems <- remDr$findElements(using = 'css selector',
                                   "div.rc")
    resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
    Sys.sleep(sample(seq(1.5,3,length.out = 10),1))
 
    emails2 <- str_match( resHeaders , "\\S*@\\S*(\\.)?") [,1]
 
    temp2 <- data.frame("Nomes" = nomes$Nome[i] ,
                        "Email" = emails2 )
 
    temp<-rbind(temp,temp2)
 
    # Terceira Pagina
    # Sys.sleep(sample(seq(1,4,length.out = 10),1))
    # 
    # css2 <- '#pnnext > span:nth-child(2)'
    # 
    # webProximo <- tryCatch({
    #   suppressMessages({
    #     remDr$findElement(using = 'css selector' ,  css2)
    #   })
    # },
    # error = function(e) {
    #   NA_character_
    # }
    # )
    # 
    # if(is.na(webProximo)) { next }
    # 
    #   webProximo$clickElement()
    # 
    #   Sys.sleep(sample(seq(1.5,4,length.out = 10),1))
    # ## Pegar textos ...
    # 
    # webElems <- remDr$findElements(using = 'css selector',
    #                                "div.rc")
    # resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
    # 
    # emails3 <- str_match( resHeaders , "\\S*@\\S*(\\.)?") [,1]
    # 
    # temp3 <- data.frame("Nomes" = nomes$Nome[i] ,
    #                     "Email" = emails3 )
    # 
    # temp<-rbind(temp,temp3)

}  


saveRDS(temp, "emails_temporarios.rds")

temp_na <- temp %>% na.omit()
ab <- readRDS( "emails_temporarios.rds")
