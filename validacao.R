## Script de Validacao
list.of.packages <- c("dplyr", "tidyr", "ggplot2", "RCurl","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RCurl)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

dl_dropbox <- function(x, key) {

  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste("O arquivo",x , "esta na pasta", getwd())))
}


dl_dropbox("ProbabilityBayes.csv","ghn6jpznwi0zgyc")
dl_dropbox("CBO2002%20-%20PerfilOcupacional.csv","s3vch0ws47dbweq")
dl_dropbox("Previsao%20Empregos.csv", "qygjhx74bipxrlo")
dl_dropbox("RAIS_CBO_1986-2017.csv", "34lp9dbgsf4kage")

results <- read.csv('ProbabilityBayes.csv')
serie   <- read.csv('RAIS_CBO_1986-2017.csv')
names(serie)[2] <- 'cbo2002'

# Calculate the total
total.cbo <- serie %>% 
             mutate(cbo2002 = factor(cbo2002)) %>% 
             group_by(cbo2002, ano) %>% 
             summarise(Total=sum(empregados))

# Calculate the median
median.cbo <- results %>% 
              group_by(COD_OCUPACAO) %>% 
              summarise(Probability=median(Probability))

colnames(median.cbo)<-c("cbo2002","Probability")

median.cbo <- median.cbo %>%
              mutate(cbo2002 = as.character(cbo2002))

# Merge data
final <- inner_join(total.cbo, median.cbo, by = "cbo2002" )

brks  <- quantile(final$Probability,probs=c(0.25,0.5,0.75))

soma2 <- final %>% 
         mutate(Classe2 = cut(Probability, 
                              c(0,brks,1),
                              labels=c("MB","B","A","MA")) ) %>%
         group_by(ano, Classe2) %>% 
         filter(ano==2017) %>% 
         summarise(emp=sum(Total))

paste("Número de empregados Alto e Muito Alto em 2017:",
      soma2[4,3]+soma2[3,3], sep = "\n") %>% cat()

paste("Percentual de Empregados Alto e Muito Alto 2017:",
      100*((soma2[4,3]+soma2[3,3])/sum(soma2[,3])) , sep = "\n") %>% cat()


### Graficos ###

# Figura 3

dl_dropbox("World%20Probability_pt.csv", "huw8obhhabkx59y")
prob <- read.csv2('World%20Probability_pt.csv')

# Remove Different approach
prob <- prob %>% 
        filter(as.character(Autor) != "Arntz, Gregory, e Zierahn (2016)")

prob$Label <-paste0(as.character(prob$Pais)," - ",as.character(prob$Autor))

mycolors    <- RColorBrewer::brewer.pal(5, "Pastel1")[5:1]
world_graph <- prob %>% 
               ggplot(aes(reorder(Label, Probabilidade), Probabilidade)) + 
               geom_col(aes(fill = Probabilidade))+  
               scale_fill_gradientn(colors = mycolors)+
               coord_flip() + 
               labs(x = "País",
                    y = "Probabilidade") +
               theme(axis.text.y=element_text(face = c(rep("plain",3), "bold",
                                                       rep("plain",13)) ) )



# Plot estatico
world_graph

# Plot interativo
ggplotly(world_graph)













##############

# forecast   <- read.csv('Previsao%20Empregos.csv')
# 
# #Calculate the total
# total.for <- forecast %>% 
#              mutate(cbo2002 = factor(cbo2002)) %>% 
#              group_by(cbo2002, ano) %>% 
#              summarise(Total=sum(empregados))
# 
# #Calculate the median
# median.cbo <- results %>% 
#   group_by(COD_OCUPACAO) %>% 
#   summarise(Probability=median(Probability))
# 
# colnames(median.cbo)<-c("cbo2002","Probability")
# 
# median.cbo <- median.cbo %>%
#   mutate(cbo2002 = as.character(cbo2002))
# 
# final3 <- inner_join(total.for, median.cbo, by = "cbo2002" )
# 
# quantile(final3$Probability)
# final3$Class<-cut(final3$Probability,breaks = c(0, 0.25, 0.5, 0.75, 1),
#                  labels = c("Muito Baixo","Baixo","Alto", "Muito Alto"))
# 
# final3 <- final3 %>% 
#           group_by(Class, ano) %>% 
#           summarise(Total=sum(Total))
# 
# table(final3$Class)
# 
# #Colors
# mycolors <- RColorBrewer::brewer.pal(4, "Pastel1")
# 
# p2<-ggplot(final3 %>% na.omit(), aes(x=ano, y=Total, colour=as.factor(Class), group=as.factor(Class))) + 
#   geom_line(aes(),lwd=1) + theme_bw() + geom_point() +
#   #scale_x_continuous(breaks = sort(unique(final$ano))[seq(1, length(unique(final$ano)), by = 10)]) +
#   scale_x_continuous(breaks = c(1990,2000,2010,2020,2030,2040),
#                      labels = c("1990","2000","2010","2020","2030","2040")) +
#   xlab("")+ ylab("Número de Empregados")+
#   scale_colour_manual(name="", values = mycolors)+
#   #theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical")+
#   scale_y_continuous(breaks = c(5000000,10000000,15000000,20000000,25000000,30000000),
#                      labels = c("5M","10M","15M","20M","25M","30M"))+
#   #scale_x_discrete(breaks =  c(1990,2000,2010,2020,2030,2040),
#   #                 labels = c("1990","2000","2010","2020","2030","2040"))+
#   guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
#   theme_minimal()+
#   theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical")
# 
# ggplotly(p2)
