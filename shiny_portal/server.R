
function(input, output, session) {
  
  botao <- reactiveValues( go = FALSE )
  traducao <- reactiveValues( ingles = FALSE )
  
  output$ui <- renderUI({withMathJax({
    singleton(
    tags$head(
      tags$meta(property="og:title", content="Automation Jobs"),
      tags$meta(property="og:type", content="site"),
      tags$meta(property="og:url", content="http://www.lamfo.shinyapps.io/automacao/"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/Cayan-Portela/cayan-portela.github.io/master/advogado.png"),
      tags$meta(property="og:image:width" , content="450"),
      tags$meta(property="og:image:height", content="298"),
      tags$meta(property="og:description",
                      content="A group of U.S. Marines, under command of
               a renegade general, take over Alcatraz and
               threaten San Francisco Bay with biological
               weapons.") )  )
    suppressDependencies("bootstrap")
    #suppressDependencies("bootstrap.min")
    suppressDependencies("htmlwidgets")
    #suppressDependencies("jquery.easing")
    suppressDependencies("jquery")
    #suppressDependencies("jquery.min")
    suppressDependencies("nivo-lightbox")
    suppressDependencies("jstree")
    suppressDependencies("custom")
    suppressDependencies("page")
    suppressDependencies("plotly")
    suppressDependencies("plotly-latest.min")
    suppressDependencies("json2")
      if ( sum(botao$go) == 0 ) {
      ##### UI code for login page
      htmlTemplate("www/index.html")
    } else { 
      fluidPage(
        #suppressDependencies("MathJax")
        div(style = "position:absolute;right:1em;",
            tags$button(
              id = "english", type = "button",
              class = "btn action-button",
              style= "color: #fff; background-color: #fff; border-color: #fff", #border-color: #2e6da4
              img(src = "http://icons.iconarchive.com/icons/wikipedia/flags/512/US-United-States-Flag-icon.png",
                  height = "30", width = "30")
            ),
            tags$button(
              id = "portugues", type = "button",
              class = "btn action-button",
              style= "color: #fff; background-color: #fff; border-color: #fff", #border-color: #2e6da4
              img(src = "http://icons.iconarchive.com/icons/wikipedia/flags/512/BR-Brazil-Flag-icon.png",
                  height = "27", width = "27")
            )
        ),
        tabsetPanel(type = "tabs",#navbarPage(
                    # useShinyjs(),
                    
                    tabPanel( ifelse(traducao$ingles,"Analysis","Análise") ,
                             htmlTemplate(
                               ifelse(traducao$ingles,"www/index2b_certo.html",
                                      "www/index2b_certo.html"),
                               botao_voltar = actionButton("voltar", label = ifelse(traducao$ingles,"Back","Voltar") ),
                               printar = tags$b(textOutput("selected_var")),
                               gauge_1 = gaugeOutput("gauge1" , height = "100px"),
                               tree = shinyTree("tree") ,#%>% withSpinner(),
                               densidade = highchartOutput('grafico',
                                                           width = "700px", height = "400px") ,#%>% withSpinner(),
                               tabela_summary = tableOutput('resumo'),
                               serie_temporal = plotlyOutput('temporal'))
                    ),
                    tabPanel(ifelse(traducao$ingles,"Text","Texto")
                             , htmlTemplate(ifelse(traducao$ingles,
                                                   "www/Paper_HTML_Simples_ingles.html",
                                                   "www/Paper_HTML_Simples.html"),
                                            Box_Paper = box_p,
                                            Serie_Paper = serie_p,
                                            Serie_Paper2 = serie_p2,
                                            world = world_graph,
                                            dispersao = plot_disp)
                    )
                    ,
                    tabPanel("Ranking",
                             htmlTemplate("www/Painel3.html",
                                          Rank2 = DT::dataTableOutput('teste')
                                          # datatable(Rank, rownames = FALSE, width = "100%",
                                          #                options = list(lengthMenu = c(50, 100, 200, 1000, 2601),
                                          #                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                                          #                               search = list(regex = TRUE, caseInsensitive = FALSE)) )
                             ) ),
                    tabPanel("Downloads",
                             htmlTemplate(  ifelse(traducao$ingles,
                                                   "www/Painel4_ingles.html",
                                                   "www/Painel4.html")  )    )
        )
      )    }
  }) })
  

  output$teste <- DT::renderDataTable({
    DT::datatable(Rank, rownames = FALSE, width = "100%", selection = 'single',
                  options = list(lengthMenu = c(50, 100, 200, 1000, 2601),
                                 language = list(url = ifelse(traducao$ingles,
                                                              '//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json',
                                                              '//cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese-Brasil.json')),
                                 search = list(regex = TRUE, caseInsensitive = FALSE) ) ) 
  })
  
  observeEvent(input$login_button,{ 
    botao$go <- TRUE 
  })
  
  observeEvent(input$voltar,{
    botao$go <- FALSE
  })
  
  observeEvent(input$english,{
    traducao$ingles <- TRUE 
  })
  
  observeEvent(input$portugues,{
    traducao$ingles <- FALSE 
  })
  
  valor <- reactiveValues(selecionado = NULL)
  
  observeEvent(input$selecionado,{ 
    valor$selecionado = input$selecionado })
  
  observeEvent(input$teste_rows_selected,{ 
    valor$selecionado = Rank[input$teste_rows_selected , 4] })
  
  output$selected_var<-renderText({
    #req(input$selecionado)
    req(valor$selecionado)
    teste <- as.character(lista$TITULO)
    if(Sys.info()[1] == "Linux") { Encoding(teste) <- 'latin1' }
    return(teste[ which(enc2native(lista$TITULO) == valor$selecionado) ])
  })
  
  output$tree <- renderTree({
    #req(input$selecionado )
    req(valor$selecionado)
    listagem( which(enc2native(lista$TITULO) == valor$selecionado ) )
  })
  
  cbo_filtrada <- reactive({
    cbo <- lista$COD_OCUPACAO[ which(enc2native(lista$TITULO) == valor$selecionado ) ]
    dados_cbo <- bayes %>% 
      dplyr::filter(COD_OCUPACAO == cbo)
    return(dados_cbo)
  })
  
  # output$tabela_dados <- DT::renderDataTable({
  #   
  #   DT::datatable( head(tabela) )
  #   
  # })
  
  output$gauge1 <- flexdashboard::renderGauge({
    
    #req(input$selecionado )
    req(valor$selecionado)
    prob <- median(cbo_filtrada()$Probability)
    #print(length(prob))
    
    # if(length( prob ) == 0) {
    #   return(NULL)
    # } else {
    legenda <- ifelse(traducao$ingles,"Probability","Probababilidade")
    flexdashboard::gauge(round( prob , 2) * 100,
                         min = 0, max = 100, symbol = '%', 
                         label = legenda,
                         flexdashboard::gaugeSectors(
                           success = c(0, 39), warning = c(40, 79), danger = c(80, 100)
                         ))
    #}
  })
  
  output$grafico <- renderHighchart({
    #print(input$teste_rows_selected)
    #req(input$selecionado)
    req(valor$selecionado)
    prob <- cbo_filtrada()$Probability
    
    hchart(prob , name = ifelse(traducao$ingles,'Histogram','Histograma') ) %>%
      hc_yAxis_multiples(
        list(lineWidth = 3 ,
             title = list(text = 'Frequência')),
        list(showLastLabel = TRUE, opposite = TRUE , 
             title = list(text = '' , rotation = 270) )
      ) %>% 
      hc_add_series( density(prob , from = 0 , to = 1) ,
                     yAxis = 1 , name = ifelse(traducao$ingles,'Density', 'Densidade') )  %>%
      hc_tooltip(valueDecimals = 2,
                 borderWidth = 3,
                 formatter = JS("function () {
                                return ' ' + this.series.name +
                                '</b> : <b>' + Highcharts.numberFormat(this.y, 2) + '</b>' +
                                '<br>' + 'Probabilidade: ' + '<b>' + Highcharts.numberFormat(this.x, 2) + '</b>';
  }") ) %>%
        hc_title( text = list( ifelse(traducao$ingles,
                                      'Automation probability distribution',
                                      'Distribuição da probabilidade de automação') ),
                  legend = '') %>%
      #  hc_subtitle(text = list( subtitulo )) %>%
      hc_add_theme(hc_theme_google()) 
    
})
  
  output$temporal <- renderPlotly({
    #req(input$selecionado)
    req(valor$selecionado)
    cbo1 <- tabela
    cbo1$serie <- as.character(cbo1$serie)
    cbo1 <- subset(cbo1 , cbo1$cbo2002 == cbo_filtrada()$COD_OCUPACAO[1] ) 
    
    cbo1$serie <- ifelse(cbo1$serie == 'Previs\xe3o' , 'Previsão' , cbo1$serie)
    cbo2 <- cbo1[ cbo1$serie == 'Previsão' , ]
    #cbo1 <- cbo1[ cbo1$serie %in% c('Original','Interpolado') , ]
    
    cbo_t <- cbo1 %>% 
      arrange(ano) %>% 
      mutate(x_next = lead(ano) , 
             y_next = lead(empregados),
             empregados = round(empregados,2))
    
    cores  <- c("#00AFBB","#FC4E07","#E7B800")
    cores2 <- c("#00AFBB","#FC4E07","#E7B800")
    names(cores)  <- c('Original' , "Interpolado","Previsão")
    names(cores2) <- c('Original' , 'Interpolated' , 'Forecast')
    
    level_serie <- unique(cbo_t[['serie']])
  #  ab <- which(c('Original' , "Interpolado","Previsão") %in% level_serie)

    
    # if(sum(traducao$ingles) == 0 ){
    #   cores <- cores[ab]
    #   names(cores) <- c('Original' , "Interpolado","Previsão")[ab]  
    #      } else {
    #   cores <- cores[ab]
    #   names(cores) <- c('Original' , 'Interpolated' , 'Forecast')[ab]
    # }
    # 
     #names(table(cbo_t$serie))
    
    #print(names(cores))
    #print(traducao$ingles)
    print(cores)
    print(cores2)
    p1 <- ggplot(data = cbo_t,
                 aes(x = ano , y = empregados )) + 
      geom_segment(aes(xend = x_next , yend = y_next,
                       colour = serie), size=0.75) + 
      labs(title = ifelse(traducao$ingles, "Series of employees numbers per year",  "Série de número de empregados por ano") , 
           #subtitle = "Profissao TAL",
           x = "" , y = "", colour = "") +
      geom_smooth(aes(x = ano  , ymax = ls , ymin = li ),
                  colour = cores[names(cores)=="Previsão"] ,#as.character(cores[ length(ab) ]),
                  fill = cores[names(cores)=="Previsão"],#as.character(cores[length(ab)]),
                  data = cbo2 , stat = 'identity' , alpha = 1/8) +
      scale_colour_manual( values = cores[names(cores) %in% level_serie] ,#ifelse(traducao$ingles, unname(cores2)[ab] , unname(cores)[ab] ), #names(cores) %in% level_serie ],
                           labels = cores[names(cores) %in% level_serie]  ) +#ifelse(traducao$ingles, names(cores2)[ab] , names(cores)[ab] ) ) + #names(cores) %in% level_serie ] ) +
      scale_y_continuous(labels = scales::comma) +
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
    #req(input$selecionado)
    req(valor$selecionado)
    prob <- cbo_filtrada()$Probability
    medidas <- c("Mínimo","Primeiro Quartil",
                 "Mediana","Média","Terceiro Quartil","Máximo","Desvio Padrão")
    measures <- c('Minimum','First Quartile','Median','Mean','Third Quartile','Maximum','Std. Deviation')
    valores <- c( unclass(summary(prob)) , sd(prob) )
    
    if(!traducao$ingles){
    tabela = data.frame( Medidas = medidas,
                         Valores = valores) }
    else {
      tabela = data.frame( Measures = measures,
                           Values   = valores)
    }
    xtable::xtable(tabela)
  })
  
  
  }