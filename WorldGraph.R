library(tidyverse)
prob<-read.csv2("Data\\World Probability.csv")

#Remove Different approach
prob <- prob %>% filter(as.character(Author) != "Arntz, Gregory, and Zierahn (2016)")
prob$Label <-paste0(as.character(prob$Country)," - ",as.character(prob$Author))

mycolors <- RColorBrewer::brewer.pal(5, "Pastel1")
mycolors<-c(mycolors,"#000000")

prob %>% 
  ggplot(aes(reorder(Label, Probability), Probability)) + 
  geom_col(aes(fill = Probability)) + 
  scale_colour_manual(name="Level of Automation",
                      values = mycolors)+ 
  coord_flip() + 
  labs(x = "Country")
ggsave('bar.pdf', units="in", width=5, height=5)
