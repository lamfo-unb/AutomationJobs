library(gsheet)
library(URLencode)
library(rvest)
library(tidyverse)
nomes<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1Isv2abfKPo__Hxxi0CpGQ6pIjS-mbzUmmHTBorA9HRY/edit?ouid=117131839543201915532&usp=sheets_home&ths=true')
nomes<-nomes[is.na(nomes$`E-mail`),]
regex<-"^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"
#Loop nomes
df<-data.frame("Nomes"="","E-mail"="")
for(i in 1:nrow(nomes)){
  link <- URLencode(paste0('https://www.google.com/search?q=',shQuote(nomes$Nome[i]),'+e-mail'))
  page <- read_html(link)
  count <- page %>% 
    html_nodes(xpath = '//*[@id="resultStats"]') %>% 
    html_text() %>% str_split(" ")
  count<-ceiling(as.numeric(count[[1]][2])/10)
  pp<-0
  for(j in 0:(count-1)){
    Sys.sleep(5)
    pp<-ifelse(j==0,0,10)+pp
    link <- URLencode(paste0('https://www.google.com/search?q=',shQuote(nomes$Nome[i]),'+e-mail&start=',pp))
    page <- read_html(link)
    results <- page %>% 
      html_nodes(xpath = '//*[@id="search"]/div') %>% 
      html_text() %>% str_extract_all("\\S*@\\S*(\\.)?") %>% unlist() 
    temp<-data.frame("Nomes"=nomes$Nome[i],"E-mail"=results)
    df<-rbind(df,temp)
  }
}
write.csv(df,"emails.csv")
