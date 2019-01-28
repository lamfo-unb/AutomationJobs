### Vale do Paraiba

dados <- haven::read_sas('C:\\Users\\b2910031\\Desktop\\vale_do_paraiba\\rais_vale_paraiba.sas7bdat')
dados <- dados %>% select(cbo2002,mun_trab)

prob <- data.table::fread('C:\\Users\\b2910031\\Downloads\\ProbabilityBayes.csv')
prob <- prob %>% 
        group_by(COD_OCUPACAO) %>%
        summarise(Mediana = median(Probability)) %>%
        arrange(Mediana)


prob2 <- prob %>% filter(COD_OCUPACAO %in% dados$cbo2002)
altos <- prob2[prob2$Mediana >= 0.6 , ]

# 
dados2 <- dados %>% filter(cbo2002 %in% altos$COD_OCUPACAO)

# Populacao de empregados formais
nrow(dados)

## Número de empregos com prob alta de automacao
nrow(dados2)

## Porcentagem de empregos com alto risco de automacao ( 71.82 % )
nrow(dados2) / nrow(dados)


## Por municipio
dados_geral <- dados %>% left_join(prob2 , by = c("cbo2002"="COD_OCUPACAO"))

ab <- tapply( dados_geral$Mediana , dados_geral$mun_trab, 
        function (x) sum(x >= 0.6 , na.rm = TRUE) / length(x) ) * 100

sum((ab %>% as.numeric()) * (table(dados$mun_trab) %>% as.numeric()))


tabela_final <- data.frame( Cod_Mun = names(ab) , 
                            Porcent = as.numeric(ab))
