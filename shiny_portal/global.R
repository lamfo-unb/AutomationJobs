library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinyTree)
library(shinyjs)
library(highcharter)
library(flexdashboard)
library(shinycssloaders)
#library(xts)
library(XML)
library(DT)


#dados <- read.csv2('dados_ficticios.csv')
tabela <- read.csv('Previsao.csv' , header = TRUE)
bayes  <- readRDS('ProbabilityBayes.rds')
lista  <- readRDS("CBO.RDS")
box_paper <- readRDS("Box_Paper.RDS")
serie_paper <- readRDS("Serie_Paper.RDS")
serie_paper2 <- readRDS("Serie_Paper2.RDS") 
Rank <- readRDS("Rank_Ocup"); Encoding(Rank$Ocupação) <- 'latin1' 
escolhas <- lista$TITULO
names(escolhas) <- `Encoding<-`(escolhas , 'latin1')

getPage <- function() {
  return(includeHTML('www2/Paper_HTML.html'))
}

listagem <- function(x){
  if(Sys.info()[1] == "Linux") { Encoding(lista$Texto) <- 'latin1' }
  a <- strsplit(lista$Texto[x], "\v")
  b <- lapply(a, strsplit, "\t")  
  c <- mapply('[', b)
  
  lista1 <- function(x){
    y <- list("")
    names(y) <- unlist(x)[1]
    y[[1]] <- structure(list(unlist(x)[2]), sticon="tags")
    names(y[[1]]) <- unlist(x)[1]
    return(y)
  }
  
  d <- sapply(c, lista1)
  
  lista2 <- function(x){
    y <- sapply(x, function(x) list(structure("", sticon="tag")) )
    return(y)
  }
  
  lista3 <- function(x){
    y <- lapply((strsplit(x[[1]], "\n")), lista2)
    y <- list(structure(unlist(y, recursive = F), sticon = "tags"))
    return(y)
  }
  
  x <- sapply(d, lista3)
  return(x)
}


mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")
mycolors<-c(mycolors, "#000000")

serie_p <- plot_ly(serie_paper, x=~year, y=~`1`, name = "Job Zone 1", marker=list(color=mycolors[1]),
             line=list(color=mycolors[1]),type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~`2`, name = "Job Zone 2", marker=list(color=mycolors[2]), line=list(color=mycolors[2])) %>%
  add_trace(y = ~`3`, name = "Job Zone 3", marker=list(color=mycolors[3]), line=list(color=mycolors[3])) %>%
  add_trace(y = ~`4`, name = "Job Zone 4", marker=list(color=mycolors[4]), line=list(color=mycolors[4]))%>%
  add_trace(y = ~`5`, name = "Job Zone 5", marker=list(color=mycolors[5]), line=list(color=mycolors[5]))%>%
  add_trace(y = ~GDP, name = "PIB Acumulado", marker=list(color =mycolors[6]), line = list(color =mycolors[6])) %>%
  layout( legend = list(orientation = 'h', xanchor = "center", x = 0.5), xaxis = list(title = " "),
          yaxis = list(title = "Taxa de Crescimento Acumulado"), hovermode = "compare")%>%
  config(displayModeBar = F)

serie_p2 <- plot_ly(serie_paper2, x=~ano, y=~`Muito Baixo`, name = "Muito Baixo", marker=list(color=mycolors[1]), 
                    line=list(color=mycolors[1]),type = 'scatter', mode = 'lines+markers') %>% 
  add_trace(y = ~Baixo, name = "Baixo", marker=list(color=mycolors[2]), line=list(color=mycolors[2])) %>%
  add_trace(y = ~`Médio`, name = "Médio", marker=list(color=mycolors[3]), line=list(color=mycolors[3])) %>%
  add_trace(y = ~Alto, name = "Alto", marker=list(color=mycolors[4]), line=list(color=mycolors[4]))%>%
  add_trace(y = ~`Muito Alto`, name = "Muito Alto", marker=list(color=mycolors[5]), line=list(color=mycolors[5]))%>%
  layout( legend = list(orientation = 'h', xanchor = "center", x = 0.5), xaxis = list(title = " "),
          yaxis = list(title = "Número de Empregados"), hovermode = "compare") %>%
  config(displayModeBar = F)

box_p <-  plot_ly(box_paper, y = ~prob, color = ~Job_Zone, type = "box") %>% 
  layout( showlegend = FALSE, xaxis = list(title = "Job Zone"),
          yaxis = list(title = "Probabilidade de Automação")) %>%
  config(displayModeBar = F)
