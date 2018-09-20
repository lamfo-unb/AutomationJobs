
function(input, output, session) {
  
  botao <- reactiveValues( go = FALSE )
  
  output$ui <- renderUI({
    
    if ( sum(botao$go) == 0 ) {
      
      ##### UI code for login page
      
      htmlTemplate("www/index.html")
    } else { 
      
      navbarPage(       #fluidPage(
        useShinyjs(),
        
        tabPanel("Painel 1" ,
                 includeCSS('www/css/bootstrap.min.css')
                 #'www/css/style.css',
                 #'ww/css/font-awesome.min.css',
                 #'www/css/nivo-lightbox.css',
                 #'www/css/nivo-lightbox-theme/default/default.css'
                 ,
                 includeCSS('www/css/style.css'),
                 includeCSS('www/css/animate.css'),
                 includeCSS('www/css/nivo-lightbox-theme/default/default.css'),
                 includeCSS('www/css/nivo-lightbox.css'),
                 includeCSS('www/font-awesome/css/font-awesome.min.css'),
                 htmlTemplate(
                   "www/index2b_certo.html",
                   printar = tags$b(textOutput("selected_var")),
                   tree = shinyTree("tree") %>% withSpinner(),
                   densidade = highchartOutput('grafico') %>% withSpinner(),
                   tabela_summary = tableOutput('resumo'),
                   serie_temporal = plotlyOutput('temporal'))
        ),
        tabPanel("Painel 2"
                 , htmlTemplate("www2/Paper_HTML_Simples.html",
                                Box_Paper = ggplotly(readRDS("Box_Paper.RDS")),
                                Serie_Paper = serie_p)
                 )
        ,
        tabPanel("Painel 3",  
                 htmlTemplate("www/Painel3.html") )
      )
      
    }
  })
  
  observeEvent(input$login_button,{ q
    botao$go <- TRUE 
  })
  
  observeEvent(input$voltar,{
    botao$go <- FALSE 
  })
  
  output$selected_var<-renderText({
    req(input$selecionado)
    teste <- as.character(lista$TITULO)
    if(Sys.info()[1] == "Linux") { Encoding(teste) <- 'latin1' }
    return(teste[ which(enc2native(lista$TITULO) == input$selecionado) ])
  })
  
  output$tree <- renderTree({
    req(input$selecionado )
    listagem( which(enc2native(lista$TITULO) == input$selecionado ) )
  })
  
  cbo_filtrada <- reactive({
    cbo <- lista$COD_OCUPACAO[ which(enc2native(lista$TITULO) == input$selecionado ) ]
    dados_cbo <- bayes %>% 
      dplyr::filter(COD_OCUPACAO == cbo)
    return(dados_cbo)
  })
  # getPage <- function() {
  #   return(includeHTML('www2/Paper_HTML.html'))
  # }
  
  output$inc <- renderUI({ withMathJax( shiny::includeHTML('www2/Paper_HTML.html') )   }) #getPage() })
  
  output$tabela_dados <- DT::renderDataTable({
    
    DT::datatable( head(tabela) )
    
  })
  
  # output$gauge <- renderGvis({
  #   df1 <- data.frame(Label = "Base Accuracy", Value = 10)
  #   gvisGauge(df1,
  #             options=list(min=0, max=100, greenFrom=0,
  #                          greenTo=29.99, yellowFrom=30, yellowTo=79.99,
  #                          redFrom=80, redTo=100, width=100, height=100,
  #                          animation = "{duration:1000,
  #                                        easing:'out'}")) 
  # })
  # 
  output$gauge1 <- flexdashboard::renderGauge({
    
    req(input$selecionado )
    prob <- cbo_filtrada()$Probability
    #print(length(prob))
    
    # if(length( prob ) == 0) {
    #   return(NULL)
    # } else {
    
    flexdashboard::gauge(round( prob , 2) * 100,
                         min = 0, max = 100, symbol = '%', 
                         label = "Probababilidade",
                         flexdashboard::gaugeSectors(
                           success = c(0, 39), warning = c(40, 79), danger = c(80, 100)
                         ))
    #}
  })
  
  output$grafico <- renderHighchart({
    req(input$selecionado)
    prob <- cbo_filtrada()$Probability
    #par2 = 4
    #x <- rbeta(1000 , shape1 = par2*prob / (1 - prob) ,
    #           shape2 = par2)
    
    hchart(prob , name = 'Histograma') %>%
      hc_yAxis_multiples(
        list(lineWidth = 3 ,
             title = list(text = 'Frequência')),
        list(showLastLabel = TRUE, opposite = TRUE , 
             title = list(text = '' , rotation = 270) )
      ) %>% 
      hc_add_series( density(prob , from = 0 , to = 1) ,
                     yAxis = 1 , name = 'Densidade' )  %>%
      hc_tooltip(valueDecimals = 2,
                 borderWidth = 3,
                 formatter = JS("function () {
                                return ' ' + this.series.name +
                                '</b> : <b>' + Highcharts.numberFormat(this.y, 2) + '</b>' +
                                '<br>' + 'Probabilidade: ' + '<b>' + Highcharts.numberFormat(this.x, 2) + '</b>';
  }") ) %>%
        hc_title( text = list('Distribuição da probabilidade de automação'),
                  legend = '') %>%
      #  hc_subtitle(text = list( subtitulo )) %>%
      hc_add_theme(hc_theme_google()) 
    
})
  
  # output$temporal <- renderHighchart({
  #   
  #   start <- as.POSIXct("2010-12-05", "%Y-%m-%d")
  #   end   <- as.POSIXct("2018-12-07", "%Y-%m-%d")
  #   datas <- seq(start, end, length.out = 200)
  #   Temporal2 <- data.frame( Valores = round( rnorm(200) , 2))
  #   rownames(Temporal2) <- datas
  #   
  #   Temporal3 <- xts::as.xts(Temporal2)
  #   
  #   highchart(type = "stock") %>% 
  #     hc_add_series(Temporal3 , id="Valores") %>%
  #     hc_title(text = list("Serie temporal da profissao"))
  # })
  
  output$temporal <- renderPlotly({
    req(input$selecionado)
    cbo1 <- tabela
    cbo1$serie <- as.character(cbo1$serie)
    cbo1 <- subset(cbo1 , cbo1$cbo2002 == cbo_filtrada()$COD_OCUPACAO[1] ) 
    
    cbo1$serie <- ifelse(cbo1$serie == 'Previs\xe3o' , 'Previsão' , cbo1$serie)
    cbo2 <- cbo1[ cbo1$serie == 'Previsao' , ]
    #cbo1 <- cbo1[ cbo1$serie %in% c('Original','Interpolado') , ]
    
    cbo_t <- cbo1 %>% 
      arrange(ano) %>% 
      mutate(x_next = lead(ano) , 
             y_next = lead(empregados),
             empregados = round(empregados))
    
    cores <- c("#00AFBB","#FC4E07","#E7B800")
    names(cores) <- c('Original' , "Interpolado","Previsão")
    
    level_serie <- names(table(cbo_t$serie))
    
    p1 <- ggplot(data = cbo_t,
                 aes(x = ano , y = empregados )) + 
      geom_segment(aes(xend = x_next , yend = y_next,
                       colour = serie), size=0.75) + 
      labs(title = "Série de numero de empregados por ano",
           #subtitle = "Profissao TAL",
           x = "" , y = "", colour = "") +
      geom_smooth(aes(x = ano  , ymax = ls , ymin = li ),
                  colour = cores[names(cores)=="Previsao"] ,
                  fill = cores[names(cores)=="Previsao"],
                  data = cbo2 , stat = 'identity' , alpha = 1/8) +
      scale_colour_manual( values = cores[ names(cores) %in% level_serie ],
                           labels = names(cores)[ names(cores) %in% level_serie ] ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom")
    
    plotly::ggplotly(p1 , tooltip = c("x","y")) %>%
      layout(legend = list(orientation = "h" , 
                           x = 0.2 , y = -0.1),
             font = list(family = 'arial')) %>%
      config(displayModeBar = F)
    
  })
  
  output$resumo <- renderTable({
    #par2 = 4
    #x <- rbeta(1000 , shape1 = par2*dados$Prob[2] / (1 - dados$Prob[2]) , shape2 = par2)
    req(input$selecionado)
    prob <- cbo_filtrada()$Probability
    medidas <- c("Mínimo","Primeiro Quartil",
                 "Mediana","Média","Terceiro Quartil","Máximo")
    tabela = data.frame( Medidas = medidas ,
                         Valores = unclass(summary(prob)))
    xtable::xtable(tabela)
  })
  
  
  }