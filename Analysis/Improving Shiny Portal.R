library(dplyr)
system.time(bayes  <- readRDS('ProbabilityBayes.rds'))
bayes2 <- bayes
bayes2$Probability <- round(bayes2$Probability, 3)
saveRDS(bayes2,"ProbabilityBayes2.rds")

system.time(bayes2  <- readRDS('ProbabilityBayes2.rds'))

head(bayes)
head(bayes2)

saveRDS(bayes2,"ProbabilityBayes.rds")



tab <- bayes %>% dplyr::group_by(COD_OCUPACAO) %>% 
  summarise(Prob = median(Probability)) %>% 
  dplyr::arrange(Prob) %>% 
  mutate(Rank = rank(Prob, ties.method = "first"))
         

tab <- left_join(tab, lista[,-3], by = "COD_OCUPACAO")
tab <- tab %>% select(Rank, Probabilidade = Prob,
                      `CBO 2002` = COD_OCUPACAO, `Ocupação` = TITULO) %>% 
  mutate(`CBO 2002` = case_when(nchar(`CBO 2002`) < 6 ~ paste0("0", `CBO 2002`),
                                  TRUE ~ paste(`CBO 2002`)))

saveRDS(tab, "Rank_Ocup")

DT::datatable(tab, rownames = FALSE, fillContainer = TRUE
          options = list(search = list(regex = TRUE, caseInsensitive = FALSE)) )
