library(purrr)
library(readr)
library(dplyr)
library(gmailr)


# Na hora de enviar, botar nomes e e-mails reais.

enderecos <- tibble(name  = c('Pedro',
                              'Cayan1',
                              'Cayan2'),
                    email = c('pedro.melo.albuquerque@gmail.com',
                              'cayanportela@hotmail.com',
                              'cayan.saavedra@ipea.gov.br'))

marcas <- tibble(name = c('Pedro','Cayan1','Cayan2'),
                 mark = 1:3)

# data.frame com valores que diferem para cada email.

my_dat2 <- left_join(enderecos, marcas)

email_sender <- 'Cayan <cayan@usp.br>' # Endereco gmail

# Se quiser copia oculta, não testei
#optional_bcc <- 'Anonymous <anon@palantir.example.org>'  

# Alterar o corpo do email com a mensagem a ser enviada. 
# '%s' são os campos que mudam, como nome da pessoa, etc.
body <- "Olá, %s.
Teste de email %s.
Link_shiny
"
edat <- my_dat2 %>%
  mutate(
    To = sprintf('%s <%s>', name, email),
    From = email_sender,
    Subject = 'Email teste',
    body = sprintf(body, name, mark)) %>%
  select(To, From, Subject, body)

edat

#write_csv(edat, "composed-emails.csv")   # Salva as informações de cada email.

emails <- edat %>%
          pmap(mime)    # leva para o formato a ser enviado por email

mensagem_segura <- safely(send_message)

# Envia.
sent_mail <- emails %>% 
             map(mensagem_segura)
