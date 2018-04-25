library(httr)
library(stringr)
library(base64enc)
library(RSelenium)

eCaps <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = getwd()
    )
    )
)

rD <- rsDriver(extraCapabilities = eCaps)

remDr <- rD$client
remDr <- remoteDriver(port = 4567L, browserName = "chrome")

remDr$open()

remDr$maxWindowSize()

site<-"http://buscatextual.cnpq.br/buscatextual/busca.do#"
remDr$navigate(site)

# Clicando em 'Busca Avançada'
webElem <- remDr$findElement(using = 'css selector',"#tit_simples > a > span")
webElem$clickElement()

# Frase exata: 'machine learning' e apertando Enter.
webElem_texto <- remDr$findElement(using = 'css selector',"#textoBuscaTodas")
webElem_texto$sendKeysToElement(list("machine learning","\uE007")) #\uE007 já manda um enter depois


# Vetor para mudar de pagina de uma em uma, de acordo com o padrao do site.
vetor <- c( 2:12 , rep(5:14,16) , 4:12 )
nomes <- list()

  for (i in vetor) {
     # Muda de pagina    
     css2 <- paste0( 'body > form > div > div.content-wrapper > div > div > div > div.layout-cell.layout-cell-12 > div > div:nth-child(2) > a:nth-child(', i, ')')
     webProximo <- remDr$findElement(using = 'css selector',
                                     css2)
     webProximo$clickElement() 
     Sys.sleep(3)
     
    # Coleta os 10 nomes de cada pagina
    for (k in 1:10) {       
      css        <- paste0('body > form > div > div.content-wrapper > div > div > div > div.layout-cell.layout-cell-12 > div > div.resultado > ol > li:nth-child(',k,')')
      Pessoa     <- remDr$findElements(using = 'css selector',
                                       css )
      nomes[[ length(nomes) + 1 ]] <- str_split(string = Pessoa[[1]]$getElementText()[[1]] , 
                                                patter = "\n" )
    }
  }
  

# Frequencia por numero de elementos nas listas
table( sapply( nomes , lengths ) )

# Fechando..
remDr$close()
rD$server$stop()

saveRDS(nomes , 'dados_lattes.rds')
