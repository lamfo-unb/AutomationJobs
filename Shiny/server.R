library(googlesheets)
library(dplyr)

table <- "responses"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

lista<-readRDS("CBO.RDS")
indRnd<- sample(1:length(lista),1,F)

shinyServer(function(input, output, session) {
#### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
            br(), br(), br(), br(),
            uiOutput("uiLogin"),
            uiOutput("pass")
          )
        )
      )
    } else {
      #### Your app's UI code goes here!
      fluidPage(
        titlePanel("Nivel de automacao"),
        fluidRow(
          column(4, wellPanel(
            sliderInput("prob", "Probabilidade de automacao:",
                        min = 0, max = 100, value = 50, step = 0.1)
          ),
          actionButton("action", label = "Enviar")
          ),
          column(8,
                 helpText("Essa e a descricao da ocupacao.\nEm uma escala de 0 a 100 determine a probabilidade de automacao.\n
                   \nQuanto maior a probabilidade, mas facil e a automatizacao\nCaso nao saiba opinar mantenha 50% como estimativa de automacao."),
                 br(),
                 br(),
                 textOutput("selected_var"),
                 br(),
                 br(),
                 verbatimTextOutput("text")
          )
        )
      )
    }
  })
  
#### YOUR APP'S SERVER CODE GOES HERE ----------------------------------------
  #Text

    output$selected_var<-renderText({
      teste <- as.character(lista$NOME_GRANDE_AREA)
      if(Sys.info()[1] == "Linux") { Encoding(teste) <- 'latin1' }
      return(teste[indRnd])
    })

     output$text <- renderText({
       teste <- lista$Texto
       if(Sys.info()[1] == "Linux") { Encoding(teste) <- 'latin1' }
       return( as.character( teste[indRnd] ) )
       })

       observeEvent(input$action,{
         df = data.frame(Usuario = credentials$user[which(credentials$user == input$user_name)], 
                         CBO_5D  = lista$COD_OCUPACAO[indRnd],
                         Prob    = input$prob)
         
         sheet_key <- '1OQYehkar5uHcSLlMu0B2C7S3TADzC8jGtE7jxCylfqk'
         sheet <- gs_key(sheet_key)
         gs_add_row(sheet, input = df)
         
       })
       
   
#### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")

  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }

    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {

      user_input$user_locked_out <- TRUE
            
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }
      
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })   

  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),

      actionButton("login_button", "Log in")
    )
  })

  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
})
