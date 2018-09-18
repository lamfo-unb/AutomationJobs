library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinyTree)
library(shinyjs)
library(highcharter)
library(flexdashboard)
#library(xts)
library(XML)
library(DT)

#dados <- read.csv2('dados_ficticios.csv')
tabela <- read.csv('Previsao.csv' , header = TRUE)
bayes  <- readRDS('ProbabilityBayes.rds')
lista  <- readRDS("CBO.RDS")

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