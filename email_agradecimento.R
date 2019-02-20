### Mandando emails de agradecimento
library(dplyr)

# Peguei a primeira coluna do google sheets do LAMFO (gmail)
respondentes <- read.table('clipboard',header = TRUE)

credentials <- readRDS('/home/cayan/Área de Trabalho/AutomationJobs/AutomationJobs/Shiny/credentials/credentials2.rds')

respondentes <- unique(respondentes)

mandar_email_para <- left_join(respondentes,credentials, by=c("Usuario"="user")) %>%
  select(Usuario,Nome) %>%
  left_join(dados , by=c("Nome"="Nomes")) %>%
  select(Usuario,Nome,email)

mandar_email_para <- mandar_email_para %>% 
                     filter( !(Usuario %in% c("LAMFO","OFMAL")) ) %>%
                     mutate(mark = 1:n())


email_sender <- 'LAMFO <lamfo.unb@gmail.com>' # Endereco gmail

# Se quiser copia oculta, não testei
#optional_bcc <- 'Anonymous <anon@palantir.example.org>'  

# Alterar o corpo do email com a mensagem a ser enviada. 
# '%s' são os campos que mudam, como nome da pessoa, etc.
body <- "Olá, %s.
Gostariamos de te agradecer pela participação na pesquisa, colaborando com nosso estudo sobre a automação das profissões no Brasil.
O trabalho pode ser acessado em https://lamfo.shinyapps.io/automacao e a apresentação dos resultados e portal será realizada no Instituto de Pesquisa Ecônomica Aplicada (IPEA) dia 03 de Dezembro às 15:00 no prédio sede em Brasília.
Obrigado pela participação.
LAMFO - Laboratório de Aprendizado de Máquina em Finanças e Organizações.
"
edat <- mandar_email_para %>%
  mutate(
    To = sprintf('%s <%s>', Nome, email),
    From = email_sender,
    Subject = 'Agradecimento',
    body = sprintf(body, Nome, mark)) %>%
  select(To, From, Subject, body)

edat

#write_csv(edat, "composed-emails.csv")   # Salva as informações de cada email.

emails <- edat %>%
  pmap(mime)    # leva para o formato a ser enviado por email

mensagem_segura <- safely(send_message)

# Envia.
sent_mail <- emails %>% 
  map(mensagem_segura)
