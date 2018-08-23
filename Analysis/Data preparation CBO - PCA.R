library(tidyverse)
library(tm)
library(stringi)
library(tidytext)
#Read the data
profile<-read.csv2("Data/Occupation Structure/CBO2002 - PerfilOcupacional.csv")
profile<-profile[,c(5,7,9)]
profile$Description<-paste(profile$NOME_GRANDE_AREA,profile$NOME_ATIVIDADE)
profile<-profile[,c(-2,-3)]

#To lower
profile<-profile %>% mutate(Description = tolower(Description))

#Remove stopwords
profile$Description<-removeWords(profile$Description, c("\\f", stopwords("portuguese")))

#Remove acents
profile$Description<-stri_trans_general(profile$Description, "Latin-ASCII")

#Count the number of words by CBO (unigram)
count1 <- profile %>%
  unnest_tokens(word, Description, token = "ngrams", n = 1)
count1 <- count1 %>%
  group_by(COD_OCUPACAO) %>% 
  count(word, sort = TRUE)

#Count the number of words by CBO (bigram)
count2 <- profile %>%
  unnest_tokens(word, Description, token = "ngrams", n = 2)
count2 <- count2 %>%
  group_by(COD_OCUPACAO) %>% 
  count(word, sort = TRUE)

#Rbind
listWords<-bind_rows(count1,count2)

#Select the words with the highest variance
stat <- listWords %>% 
            group_by(word) %>% 
            summarise(variance = var(n),
                      Mean = mean(n)) 
stat[ is.na(stat) ] <- 0 
stat$CV <- sqrt(stat$variance)/stat$Mean
stat[ is.na(stat) ] <- 0 
hist(stat$CV)
quantile(stat$CV,probs=seq(0,1,length.out = 100))

#Keep the top 5%
statCV <-data.frame(word=stat$word[which(stat$CV>0.0)])
statCV$word<-as.character(statCV$word)

#Merge with listWords
listWords$word<-as.character(listWords$word)
listWordsFilter<- merge(listWords,statCV,by="word",all=F)

#Check if all CBO´s are here
length(unique(listWords$COD_OCUPACAO))
length(unique(listWordsFilter$COD_OCUPACAO))

#Create the data matrix
dataWords<- reshape2::dcast(listWordsFilter, COD_OCUPACAO ~ word, sum, fill=0) 

# advisable, but default is FALSE. 
pca <- prcomp(dataWords[,2:19645],
                 center = TRUE,
                 scale = TRUE) 
vars <- apply(pca$x, 2, var)  
props <- vars / sum(vars)
head(cumsum(props),1000)

pcaWords<-predict(pca, newdata=dataWords[,2:19645]) #90% of explained variance
pcaWords<-pcaWords[,1:696]
pcaWords<-data.frame(COD_OCUPACAO=dataWords$COD_OCUPACAO,pcaWords)

#Create the correlation plot
saveRDS(pcaWords,"Data/CBOPCAwords.rds")
rm(list=setdiff(ls(), "pca"))
save.image("Data/PCA.RData")



#############################################################################
rm(list=ls())
load("Data/PCA.RData")

library(factoextra)
eig.val <- get_eigenvalue(pca)
fviz_screeplot(pca, ncp=1000, choice="variance", axis.ticks = element_blank())
fviz_screeplot(res.pca, ncp=10, choice="eigenvalue")