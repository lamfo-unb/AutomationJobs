library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinyTree)
#library(shinyjs)
library(highcharter)
library(flexdashboard)
library(shinycssloaders)
#library(xts)
library(XML)
library(DT)
library(readxl)
#dados <- read.csv2('dados_ficticios.csv')
tabela <- read.csv('Previsao.csv' , header = TRUE)
bayes  <- readRDS('ProbabilityBayes.rds')
lista  <- readRDS("CBO.RDS")
box_paper <- readRDS("Box_Paper.RDS")
serie_paper <- readRDS("Serie_Paper.RDS")
serie_paper2 <- readRDS("serie_paper2_4niveis.rds") 
Rank <- readRDS("Rank_Ocup2.rds")#; 
Encoding(Rank[,4]) <- 'latin1' 
Encoding(names(Rank)) <- 'latin1'
names(Rank)[1] <- "Posto"
#Rank$Probabilidade <- sprintf("%.5f", Rank$Probabilidade)
escolhas <- lista$TITULO
#escolhas <- enc2native(escolhas)
Encoding(lista$TITULO) <- 'latin1'
Encoding(escolhas) <- 'latin1'

## Retirando 'Mae social' (esta estranho)
#which(Rank[,4] == "Mãe Social") (2492)
#which(escolhas == "Mãe social") (1381)
Rank <- Rank[-2492,]
escolhas <- escolhas[-1381]

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
 # add_trace(y = ~`Médio`, name = "Médio", marker=list(color=mycolors[3]), line=list(color=mycolors[3])) %>%
  add_trace(y = ~Alto, name = "Alto", marker=list(color=mycolors[3]), line=list(color=mycolors[3]))%>%
  add_trace(y = ~`Muito Alto`, name = "Muito Alto", marker=list(color=mycolors[4]), line=list(color=mycolors[4]))%>%
  layout( legend = list(orientation = 'h', xanchor = "center", x = 0.5), xaxis = list(title = " "),
          yaxis = list(title = "Número de Empregados"), hovermode = "compare") %>%
  config(displayModeBar = F)

box_p <-  plot_ly(box_paper, y = ~prob, color = ~Job_Zone, type = "box") %>% 
  layout( showlegend = FALSE, xaxis = list(title = "Job Zone"),
          yaxis = list(title = "Probabilidade de Automação")) %>%
  config(displayModeBar = F)


prob<-read.csv2('World Probability_pt.csv')
#Remove Different approach
prob <- prob %>% filter(as.character(Author) != "Arntz, Gregory, e Zierahn (2016)")
prob$Label <-paste0(as.character(prob$Country)," - ",as.character(prob$Author))

mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")[5:1]
world_graph <- prob %>% 
  ggplot(aes(reorder(Label, Probability), Probability)) + 
  geom_col(aes(fill = Probability))+  
  scale_fill_gradientn(colors = mycolors)+
  coord_flip() + 
  labs(x = "Country") +
  theme(axis.text.y=element_text(face = c(rep("plain",3), "bold",
                                          rep("plain",13)) ) )

world_graph <- ggplotly(world_graph)



#### dispersao

cbo_cni <- read_excel('Para Cayan_ Ocupações Sistema Indústria.xlsx')

probs_disp   <- bayes %>%
  group_by(COD_OCUPACAO) %>%
  summarise(Probabilidade = median(Probability))

tabela1 <- tabela %>%
  inner_join(cbo_cni, by = c('cbo2002' = 'COD_CBO6')) %>%
  filter(ano %in% 2000:2017 & serie == 'Original') %>%
  mutate(cbo2002 = as.character(cbo2002)) %>%
  group_by(cbo2002) %>%
  summarise(Taxa = log(empregados[n()]  / empregados[1]) ,
            empregados = empregados[n()],
            Descricao = DESC_CBO6[1])

probs_cni2 <- bayes %>% 
              inner_join(tabela1, by = c('COD_OCUPACAO' = 'cbo2002')) %>%
              distinct(COD_OCUPACAO, .keep_all = T)

probs_cni2 <- probs_cni2 %>%
              mutate(Quadrante = ifelse(Probability >= 0.5 & Taxa >= 1, "Primeiro",
                                 ifelse(Probability <= 0.5 & Taxa >= 1, "Segundo",
                                 ifelse(Probability <= 0.5 & Taxa <= 1, "Terceiro",
                                        "Quarto") ) ))

plot_disp <- ggplot() +
             geom_point(aes( x = Probability , y = Taxa, label = Descricao,
                             size = empregados, colour = Quadrante), alpha = 0.5,
                        data = probs_cni2 ) +
             labs(y = "log taxa") +
             geom_hline(yintercept = 1 , color = 'firebrick', linetype = 2) +
             geom_vline(xintercept = 0.5 , color = 'firebrick', linetype = 2) +
             guides(colour = FALSE, size = FALSE) +
             theme_minimal()

plot_disp <- ggplotly(plot_disp)
