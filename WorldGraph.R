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
