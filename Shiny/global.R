library(shiny)
library(dplyr)
library(digest)
library(googlesheets)


lista<-readRDS("CBO.RDS")
credentials <- readRDS("credentials/credentials2.rds")

gs_auth(token = "googlesheets_token2.rds")
sheet_key <- '1OQYehkar5uHcSLlMu0B2C7S3TADzC8jGtE7jxCylfqk'
sheet <- gs_key(sheet_key)

############# FOR TESTING ONLY - DELETE THIS CODE before using app!!!

# credentials data frame for testing (username = "test" and password = "password")
# NOTE: in real use this would open an existing data frame containing user credentials

  if (!file.exists("credentials/credentials2.rds")) {
    credentials <- data.frame(user = "test", 
                              pw = "380796939c86c55d6aa8ea8c941f7652", 
                              locked_out = FALSE,
                              stringsAsFactors = FALSE)
    
    saveRDS(credentials, "credentials/credentials.rds")
  }
    
############# CODE ABOVE FOR TESTING ONLY - DELETE before using app!!!
  
# set the number of failed attempts allowed before user is locked out

num_fails_to_lockout <- 3

# ==================================================================================
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
