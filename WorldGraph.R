library(tidyverse)
prob<-read.csv2("Data\\World Probability.csv")

#Remove Different approach
prob <- prob %>% filter(as.character(Author) != "Arntz, Gregory, and Zierahn (2016)")
prob$Label <-paste0(as.character(prob$Country)," - ",as.character(prob$Author))

mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")[5:1]
prob %>% 
  ggplot(aes(reorder(Label, Probability), Probability)) + 
  geom_col(aes(fill = Probability))+  
  scale_fill_gradientn(colors = mycolors)+
  coord_flip() + 
  labs(x = "Country") +
  theme(axis.text.y=element_text(face = c(rep("plain",3), "bold",
                                          rep("plain",13)) ) )
ggsave('bar.pdf', units="in", width=8, height=5)

# PT version
prob_pt<-read.csv2("Data\\World Probability_pt.csv")

#Remove Different approach
prob_pt <- prob_pt %>% filter(as.character(Author) != "Arntz, Gregory, e Zierahn (2016)")
prob_pt$Label <-paste0(as.character(prob_pt$Country)," - ",as.character(prob_pt$Author))
names(prob_pt)[3] <- 'Probabilidade'
mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")[5:1]
prob_pt %>% 
  ggplot(aes(reorder(Label, Probabilidade), Probabilidade)) + 
  geom_col(aes(fill = Probabilidade))+  
  scale_fill_gradientn(colors = mycolors)+
  coord_flip() + 
  labs(x = "País") +
  theme(axis.text.y=element_text(face = c(rep("plain",3), "bold",
                                          rep("plain",13)) ) )
ggsave('bar_pt.pdf', units="in", width=8, height=5)
