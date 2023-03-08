#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shinyWidgets")
# install.packages("fuzzyjoin")
# install.packages("DT")
# install.packages("shinyjs")
# install.packages('rsconnect')
# install.packages('stringdist')
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(janitor)
library(readxl)
library(jsonlite)
library(DT)
library(shinyjs)
# setwd("C:/Users/jdrdionisio/Desktop/RProjects/Projects/INE/INE_API")

# geolinkage_path <- "datasets/geolinkage_aces_2022.csv"
geolinkage_aces <- read_csv("datasets/geolinkage_aces_2022.csv", col_types = cols(.default = "c"), locale = locale("pt"))

geolinkage_aces <- geolinkage_aces%>%
  filter(!dicofre_2013 %in% c("0", "999999"))

geo_ref_df <- list(geolinkage_aces,
                   geolinkage_aces[2:20],
                   geolinkage_aces[2:20],
                   geolinkage_aces[c(7:20)],
                   geolinkage_aces[c(7:20)],
                   geolinkage_aces[c(10:15,19,20)],
                   geolinkage_aces[c(12:15)])

Indicadores <- read_excel("datasets/Indicadores.xlsx", 
             skip = 14)%>%
  clean_names()%>%
  filter(disponivel_no_portal== "Sim")%>%
  distinct(designacao, .keep_all = TRUE)

counter <- 1

result_list <- list()

sleep <- function(z){
  if (z %% 100 == 0) {
    Sys.sleep(5)
    z <- 1
  }else{
    z <- z+1
  }
  return(z)
  }

ine.get <- function(indicadores,largest_area,obs_back,result_list,geo_ref_df) {
  a <- length(indicadores)
  dicofre_2013 <- unique(largest_area$dicofre_2013)
  municipio_2013 <- unique(largest_area$municipio_2013_cod)
  municipio_2002 <- unique(largest_area$municipio_2002_cod)
  nuts_3_2013 <- unique(largest_area$nuts3_2013_cod)
  nuts_3_2002 <- unique(largest_area$nuts3_2002_cod)
  nuts_2_2013 <- unique(largest_area$nuts2_2013_cod)
  nuts_1 <- unique(largest_area$nuts1_2013_cod)
  counter <- 0
  #Existem 2 desagregacoes que estao hard-coded porque sao iguais entre 2013 e 2002
  testd <- c("&Dim2=011102&lang=PT",  #Testa a desagregação por freguesias
             "&Dim2=16E0111&lang=PT",#Testa a desagregação por municipio 2013
             "&Dim2=1610111&lang=PT",#Testa a desagregação por municipio 2002
             "&Dim2=16E&lang=PT",    #Testa a desagregação por NUTSIII 2013
             "&Dim2=161&lang=PT",    #Testa a desagregação por NUTSIII 2002
             "&Dim2=16&lang=PT",     #Testa a desagregação por NUTSII 2013
             "&lang=PT" )                     #Testa a desagregação por NUTSI ou sem padrão )
  codes_list <- list(dicofre_2013,municipio_2013,municipio_2002,nuts_3_2013,nuts_3_2002,nuts_2_2013,"")
  b_list <- list(length(dicofre_2013),length(municipio_2013),length(municipio_2002),length(nuts_3_2013),length(nuts_3_2002),length(nuts_2_2013),1)
  agreg_list <- list("Freguesia","Municipio","Municipio","NUTSIII","NUTSIII","NUTSII","Nacional")
  desag_v <- c("&Dim2=","&Dim2=","&Dim2=","&Dim2=","&Dim2=","&Dim2=","")
  geo_ref_df <- geo_ref_df
  geo_ref_by_v <- c("dicofre_2013",
                    "municipio_2013_cod",
                    "municipio_2002_cod",
                    "nuts3_2013_cod",
                    "nuts3_2002_cod",
                    "nuts2_2013_cod",
                    "nuts1_2013_cod")
  for (i in 1:a) {
    #COMECA POR LER O INDICADOR A RETIRAR
    indicador_atual <- indicadores[i]
    counter <- sleep(counter)
    test <- list()
    for (k in 1:length(testd)){
      counter <- sleep(counter)
      test[[k]] <- as.data.frame(fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T",testd[k])))
    }
    r <- 1
    b <- b_list[[1]]
    codes <- codes_list[[1]]
    agreg <- agreg_list[[1]]
    desag <- desag_v[1]
    geo_ref_d <- geo_ref_df[[1]]
    geo_ref_by <- geo_ref_by_v[1]
    while("Falso" %in% colnames(test[[r]]$Sucesso)){
      r <- r + 1
      b <- b_list[[r]]
      codes <- codes_list[[r]]
      agreg <- agreg_list[[r]]
      desag <- desag_v[r]
      geo_ref_d <- geo_ref_df[[r]]
      geo_ref_by <- geo_ref_by_v[r]
    }
    for (w in 1:b){
      counter <- sleep(counter)
      result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T", desag, codes[w],"&lang=PT"))
      dados <- as.data.frame(result$Dados)
      colunas_nanos <- colnames(dados)
      num_colunas <- length(colunas_nanos)
      if(obs_back > num_colunas){
        x <-num_colunas
      }else{
        x <-obs_back
      }
      # Get column names for years of interest
      obs_cols <- as.character(c(colunas_nanos[(num_colunas-x+1):(num_colunas)]))
      df_all <- data.frame()
      # Loop over years
      for (obs in obs_cols) {
        # Remove all columns except current year
        df <- dados %>%
          unnest(!!sym(obs))%>%
          select(-any_of(colunas_nanos))%>%
          mutate(obs=as.character(obs))%>%
          left_join(geo_ref_d, by=c("geocod" = as.character(geo_ref_by)),  multiple = "first" )
        # Add data frame to list
        df_all <- bind_rows(df_all, df)
        # Pause for 1 second
        Sys.sleep(1)
      }
      if(w==1){
        result_list[[indicador_atual]] <- df_all
      }
      else{
        result_list[[indicador_atual]] <- bind_rows(result_list[[indicador_atual]], df_all)
      }
    }
  }
  return(result_list)
}

desag_opcoes <- NULL
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  useShinyjs(),
  fluidRow(
    column(
      width = 6,
      align = "center",
      height = 60,
      imageOutput("sns_img",  height = "60px")
    ),
    column(
      width = 6,
      align = "center",
      height = 60,
      imageOutput("dgs_img",  height = "60px")
    )
  ),
  titlePanel(
    h1("Extrator de Dados do INE", align= "left")
    ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3(strong("Autoria: João Dionísio")),
      h4(strong("Apoio: Rafael Vasconcelos")),
      br(),
      br(),
      sliderInput("obs_back",
                  "Número de observações a pedir:",
                  min = 1,
                  max = 30,
                  value = 2),
      br(),
      p("Observações - para inclusão de indicadores com peridicidade não-anual 
        - retira o pedido ou o máximo possível"),
      # Search bar for dataset
      selectizeInput("dataset",
                     "Selecionar ou Pesquisar indicadores:",
                     choices= NULL,
                     # choices = Indicadores$designacao,
                     multiple = TRUE
                     ),
      p("Indicadores disponíveis dia 6 de Março"),
      br(),
      # Search bar for desagregação
      selectInput("desagregacao",
                  "Menor desagregação pretendida:",
                  choices = c("ACES", "ARS", "Nacional", "Municipio", "Freguesia","Continente")),
      # Dropdown for additional desagregação options
      uiOutput("desagregacao_opcoes"),
      p("É procurada sempre a menor desagregação possível"),
      uiOutput("my_checkbox"),
      uiOutput("desagregacao_opcoes_sup"),
      actionButton("go", "Submeter",class = "btn-primary"),
      checkboxInput("my_checkbox1",
                    "Debug Panel", 
                    FALSE),
      br(),
      p("Próximas melhorias:"),
      p("Automatizar a procura dos Indicadores disponíveis"),
      p("Incluir a possibilidade de incluir os metadados"),
      p("Dar opções de agrupar as variáveis"),
      p("Visualizar por opções escolhidas os 
      dataframes finais")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      h4("Painel Debug"),
      uiOutput("debug_panel"),
      h2("Dados Recolhidos pelo Extractor"),
      uiOutput("error"),
      uiOutput("real_data_tabs"),
      h1("Dados Agregados")
      # uiOutput("agg_data_tabs")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$sns_img <- renderImage({
    list(src = "www/SNS.png",
         height= 60)
    
  }, deleteFile = F)
  output$dgs_img <- renderImage({
    
    list(src = "www/DGS.png",
         height= 60)
    
  }, deleteFile = F)
  # Update the choices for the dataset input based on the text input
    updateSelectizeInput(session, 
                          'dataset', 
                          choices = Indicadores$designacao,
                          options = list(
                          placeholder = 'Barra de Pesquisa',
                          create = FALSE
                          ,maxOptions = 15)
                          ,server = TRUE)
  # Create a reactive function for the desagregacao options
  desag_opcoes <- reactive({
    opcoes <- NULL
# ligado ao input desagregacao podem se adicionar os NUTS III e NUTS II
    if(input$desagregacao == "ACES") {
      opcoes <- geolinkage_aces$aces_2022
    } else if(input$desagregacao == "ARS") {
      opcoes <- geolinkage_aces$ars_2022
    } else if(input$desagregacao == "Município") {
      opcoes <- geolinkage_aces$municipio_2013
    } else if(input$desagregacao == "Freguesia") {
      opcoes <- geolinkage_aces$freguesia_2013
    } else if(input$desagregacao == "Continente") {
      opcoes <- geolinkage_aces$nuts1_2013
    } 
    return(opcoes)
  })
  # Create the dynamic dropdown menu for desagregacao_opcoes
  output$desagregacao_opcoes <- renderUI({
    if(!is.null(desag_opcoes())) {
      selectizeInput("area",
                     "Selecionar um filtro de área:",
                     # choices = NULL, 
                     choices = desag_opcoes(),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Barra de pesquisa',
                       create = FALSE)
      )
    }
  })
  # Checkbox input
  output$my_checkbox <- renderUI({
    if (input$desagregacao!="Nacional") {
      checkboxInput("my_checkbox",
                    "Buscar dados de desagregações superiores", 
                    FALSE)
    } else {
      NULL
    }
  })
  
  output$debug_panel <- renderUI({
    if (input$my_checkbox1==TRUE) {
      tabsetPanel(
        tabPanel("Tabela", DT::dataTableOutput("df")),
        tabPanel("Número de Indicador", verbatimTextOutput("filtered_ind")),
        tabPanel("Códigos Freguesia", verbatimTextOutput("filtered_ind1")),
        tabPanel("Códigos 2013", verbatimTextOutput("filtered_ind2")),
        tabPanel("Códigos 2002", verbatimTextOutput("filtered_ind3")),
        tabPanel("Grupos", verbatimTextOutput("opcoes")),
        tabPanel("Desagregação", verbatimTextOutput("opcoes_1"))
      )
    } else {
      NULL
    }
  })
  # Create a reactive function for the desagregacao options
  desag_opcoes_sup <- reactive({
    opcoes_1 <- NULL
    
    if(input$desagregacao == "ACES") {
      opcoes_1 <- c("Nacional","Continente","ARS")
    } else if(input$desagregacao == "ARS") {
      opcoes_1 <- c("Nacional", "Continente")
    } else if(input$desagregacao == "Municipio") {
      opcoes_1 <- c("Nacional", "Continente","ARS","ACES")
    } else if(input$desagregacao == "Freguesia") {
      opcoes_1 <- c("Nacional", "Municipio", "Continente", "ARS","ACES")
    }
    return(opcoes_1)
  })
  
  output$desagregacao_opcoes_sup <- renderUI({
    if(!is.null(desag_opcoes())& input$my_checkbox == TRUE){
      selectizeInput("area1",
                     "Selecionar área:",
                     choices = desag_opcoes_sup(),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Barra de pesquisa',
                       create = FALSE)
      )
    }
  })
  # Create a reactive function for the filtered dataset
  filtered_desag <- reactive({
    if(input$my_checkbox==FALSE & "Nacional" %in% input$desagregacao){
        filtered <-  geolinkage_aces
        filtered1 <- filtered$dicofre_2013
        filtered2 <- unique(filtered$municipio_2013_cod)
        filtered3 <- unique(filtered$municipio_2002_cod)}
    else if(input$my_checkbox==FALSE){
      filtered <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
      filtered1 <- filtered$dicofre_2013
      filtered2 <- unique(filtered$municipio_2013_cod)
      filtered3 <- unique(filtered$municipio_2002_cod)}
    else if(input$my_checkbox==TRUE){
      if("Nacional" %in% input$area1){
        filtered <-  geolinkage_aces
        filtered1 <- filtered$dicofre_2013
        filtered2 <- unique(filtered$municipio_2013_cod)
        filtered3 <- unique(filtered$municipio_2002_cod)}
      else if("Continente" %in% input$area1){
        extra_prep <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
        extra <- unique(extra_prep$nuts1_2013)
        filtered <- subset(geolinkage_aces, nuts1_2013 %in% extra)
        filtered1 <- filtered$dicofre_2013
        filtered2 <- unique(filtered$municipio_2013_cod)
        filtered3 <- unique(filtered$municipio_2002_cod)}
     else if("ARS" %in% input$area1){
       extra_prep <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
       extra <- unique(extra_prep$ars_2022)
       filtered <- subset(geolinkage_aces, ars_2022 %in% extra)
       filtered1 <- filtered$dicofre_2013
       filtered2 <- unique(filtered$municipio_2013_cod)
       filtered3 <- unique(filtered$municipio_2002_cod)}
      else if("ACES" %in% input$area1){
        extra_prep <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
        extra <- unique(extra_prep$aces_2022)
        filtered <- subset(geolinkage_aces, aces_2022 %in% extra)
        filtered1 <- filtered$dicofre_2013
        filtered2 <- unique(filtered$municipio_2013_cod)
        filtered3 <- unique(filtered$municipio_2002_cod)}
      else if("Municipio" %in% input$area1){
        extra_prep <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
        extra <- unique(extra_prep$municipio_2013)
        filtered <- subset(geolinkage_aces, municipio_2013 %in% extra)
        filtered1 <- filtered$dicofre_2013
        filtered2 <- unique(filtered$municipio_2013_cod)
        filtered3 <- unique(filtered$municipio_2002_cod)}
      else{
      filtered <- subset(geolinkage_aces, desag_opcoes() %in% input$area)
      filtered1 <- filtered$dicofre_2013
      filtered2 <- unique(filtered$municipio_2013_cod)
      filtered3 <- unique(filtered$municipio_2002_cod)}}
    return(list(filtered_table = filtered, filtered1 = filtered1, filtered2 = filtered2, filtered3 = filtered3))
  })
  # Create a reactive function for the filtered dataset
  filtered_indicador <- reactive({
    filtered <- subset(Indicadores, designacao %in% input$dataset)
    filtered_ind <- filtered$codigo_de_difusao
    return(filtered_ind)
  })
  # Output the filtered dataset to the "df" table
  output$df <- DT::renderDataTable({
    filtered_desag()$filtered_table}
  )
  # Output the filtered codigo_de_difusao to "filtered_ind"
  output$filtered_ind <- renderPrint({
    filtered_indicador()
  })
  output$filtered_ind1 <- renderPrint({
    filtered_desag()$filtered1
  })
  output$filtered_ind2 <- renderPrint({
    filtered_desag()$filtered2
  })
  output$filtered_ind3 <- renderPrint({
    filtered_desag()$filtered3
  })
  output$opcoes <- renderPrint({
    input$area1
  })
  opcoes_reactive <- reactiveVal()
  output$opcoes_1 <- renderPrint({
    c(input$area1,input$desagregacao)
  })

  result_list_reactive <- reactiveVal()
  
  desag <- reactiveVal()
  
  observeEvent(input$go,{
    # Disable inputs
    shinyjs::disable(selector = "input")
    shinyjs::disable(selector = "select")
    shinyjs::disable(selector = "button")
    result_list <- result_list
    # extract data from INE using the inputs from the UI
    if(length(filtered_indicador())!=0&nrow(filtered_desag()$filtered_table)!=0){
    result_list_updated <- ine.get(indicadores = filtered_indicador(), 
            largest_area = filtered_desag()$filtered_table, 
            obs_back = input$obs_back, 
            result_list= result_list,
            geo_ref_df = geo_ref_df)
    
    result_list_reactive(result_list_updated)
    print(result_list_updated)
    desag(c(input$area1,input$desagregacao))
    output$error <- NULL}
    else if(length(filtered_indicador())==0){
      output$error <- renderUI({
        tagList(
        br(),
        h1(strong("Não foi pedido nenhum indicador")),
        br())
      })
    }
    else {
      output$error <- renderUI({
        tagList(
        br(),
        h1(strong("Não foi pedida nenhuma desagregação")),
        br())
      })
    }
    # Enable inputs
    shinyjs::enable(selector = "input")
    shinyjs::enable(selector = "select")
    shinyjs::enable(selector = "button")
  })
  # Render the tabs based on the reactive value
  output$real_data_tabs <- renderUI({
    # Get the items from result_list_reactive
    items <- names(result_list_reactive())
    # Create a list of tabPanels with dataTables and downloadButtons
    tabs <- lapply(items, function(item) {
      full_name <- Indicadores$designacao[Indicadores$codigo_de_difusao == item]
      title <- substr(Indicadores$designacao[Indicadores$codigo_de_difusao == item], 1, 10)
      if (length(full_name) == 0) {
        full_name <- item
      } else {
        full_name <- full_name[1]
      }
      tabPanel(
        title,# set tooltip with full name
        h4(strong(full_name)),
        DT::dataTableOutput(paste0(item, "_table")),
        downloadButton(paste0(item, "_download"), paste0(items, ".csv"))
      )
    })
    # Return a tabsetPanel with the tabs
    do.call(tabsetPanel, tabs)
  })
  
  # Render dataTables and download handlers when result_list_reactive changes
  observe({
    lapply(names(result_list_reactive()), function(item) {
      output[[paste0(item, "_table")]] <- DT::renderDataTable({
        # Get the data from result_list_reactive
        data <- result_list_reactive()[[item]]
        # Return a dataTable with the data
        DT::datatable(data)
      })
      output[[paste0(item, "_download")]] <- downloadHandler(
        filename = function() {
          paste0(item, ".csv")
        },
        content = function(file) {
          # Get the data from result_list_reactive
          data <- result_list_reactive()[[item]]
          # Write the data to a csv file
          write.csv(data, file)
        }
      )
    })
  })
}
# Run the application 
shinyApp(ui, server)


#aggreagated data
# output$agg_data_tabs <- renderUI({
#   # Get the items from result_list_reactive
#   items <- names(result_list_reactive())
#   # Create a list of tabPanels with dataTables and downloadButtons
#   tabs <- list()
#   # Create a list to hold the data frames grouped by the column mentioned in the if statements
#   group_list <- list(
#     ACES = list(),
#     ARS = list(),
#     Freguesia = list(),
#     Municipio = list(),
#     Continente = list(),
#     Nacional = list()
#   )
#   # Loop over each item in result_list_reactive
#   for (item in items) {
#     full_name <- Indicadores$designacao[Indicadores$codigo_de_difusao == item]
#     title <- substr(Indicadores$designacao[Indicadores$codigo_de_difusao == item], 1, 20)
#     if (length(full_name) == 0) {
#       full_name <- item
#     } else {
#       full_name <- full_name[1]
#     }
#     # Loop over each aggregation level
#     for (agg in desag()) {
#       # Group the data based on the column mentioned in the if statements
#       if ("ACES" %in% input$desagregacao | "ACES" %in% input$area1& "aces_2022" %in% colnames(result_list_reactive()[[item]])) {
#         group_list$ACES[[agg]] <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "aces_2022"] %in% agg, ]
#       }
#       if ("ARS" %in% input$desagregacao | "ARS" %in% input$area1& "ars_2013_cod" %in% colnames(result_list_reactive()[[item]])) {
#         group_list$ARS[[agg]] <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "ars_2013_cod"] %in% agg, ]
#       }
#       if ("Freguesia" %in% input$desagregacao | "Freguesia" %in% input$area1 & "dicofre_2013" %in% colnames(result_list_reactive()[[item]])) {
#         group_list$Freguesia[[agg]] <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "dicofre_2013"] %in% agg, ]
#       }
#       if ("Municipio" %in% input$desagregacao | "Municipio" %in% input$area1 & "municipio_2013" %in% colnames(result_list_reactive()[[item]])) {
#         group_list$Municipio[[agg]] <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "municipio_2013"] %in% agg, ]
#       }
#       if ("Continente" %in% input$desagregacao | "Continente" %in% input$area1& "nuts1_2013_cod" %in% colnames(result_list_reactive()[[item]])) {
#         group_list$Continente[[agg]] <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "nuts1_2013_cod"] %in% agg, ]
#       }
#       if ("Nacional" %in% input$desagregacao | "Nacional" %in% input$area1) {
#         group_list$Nacional[[agg]] <- result_list_reactive()[[item]]
#       }
#     }
#     # Loop over each if statement that is TRUE and create
#     
#       # filter the data based on indicator and aggregation level
#       data <- result_list_reactive()[[item]][result_list_reactive()[[item]][, "Aggregation"] %in% agg, ]
#       # add the aggregation level to the download button name
#       download_name <- paste0(item, "_", agg, ".csv")
#       tabs <- append(tabs, list(tabPanel(
#         title, # set tooltip with full name
#         h4(strong(full_name)),
#         DT::dataTableOutput(paste0(item, "_", agg, "_table")),
#         downloadButton(paste0(item, "_", agg, "_download"), download_name)
#       )))
#       # Render dataTables and download handlers for the current indicator and aggregation level
#       output[[paste0(item, "_", agg, "_table")]] <- DT::renderDataTable({
#         # Return a dataTable with the data
#         DT::datatable(data)
#       })
#       output[[paste0(item, "_", agg, "_download")]] <- downloadHandler(
#         filename = function() {
#           download_name
#         },
#         content = function(file) {
#           # Write the data to a csv file
#           write.csv(data, file)
#         }
#       )
#     }
#   }
#   # Return a tabsetPanel with the tabs
#   do.call(tabsetPanel, tabs)
# })
# 
# # Render dataTables and download handlers when result_list_reactive changes
# observe({
#   lapply(names(result_list_reactive()), function(item) {
#     output[[paste0(item, "_table")]] <- DT::renderDataTable({
#       # Get the data from result_list_reactive
#       data <- result_list_reactive()[[item]]
#       # Return a dataTable with the data
#       DT::datatable(data)
#     })
#     output[[paste0(item, "_download")]] <- downloadHandler(
#       filename = function() {
#         paste0(item, "_", input$area1, ".csv")
#       },
#       content = function(file) {
#         # Get the data from result_list_reactive
#         data <- result_list_reactive()[[item]]
#         # Filter the data based on the selected deaggregations
#         data <- data %>% filter(desagregacao %in% input$area1)
#         # Write the data to a csv file
#         write.csv(data, file)
#       }
#     )
#   })
# })
# Extração da metainformação:
#   
# {host_url}/ine/json_indicador/pindicaMeta.jsp?varcd={varcd_cod}&lang={lang}
# host_url: -> Endereço host do website do INE, https://www.ine.pt
# varcd_cod -> Código do indicador
# lang: -> Língua pretendida para o resultado da extração (“PT” or “EN”)
# 
# Exemplo: https://www.ine.pt/ine/json_indicador/pindicaMeta.jsp?varcd=0001234&lang=PT
# 
#   file_names <- list.files(path = "outputs", pattern = ".*\\.csv")
# 
#   # Initialize a list to store the data frames
#   df_list <- list()
#   #FOR LOOP
#   for (file_name in file_names) {
#     # Split the file name into its components
#     components <- strsplit(file_name, c("_"))[[1]]
# 
#     # Remove the ".csv" extension from the last component
#     last_component <- sub(".csv", "", components[length(components)])
#     components[length(components)]
#     # Replace the last component in the components vector
#     components[length(components)] <- last_component
#     # Assign the components to variables
#     indicador <- components[1]
#     year <- components[2]
#     aggregation <- components[3]
# 
#     # Load the file into a data frame
#     df <- read.csv(file.path("outputs", file_name))
# 
#     # Add columns for year and aggregation
#     df <- cbind(df, year = year, aggregation = aggregation)
# 
#     # Check if a data frame for this indicator already exists
#     if (!(indicador %in% names(df_list))) {
#       # If not, create a new data frame for this indicator
#       df_list[[indicador]] <- df
#     } else {
#       # If it does, use rbind to combine the data frames
#       df_list[[indicador]] <- rbind(df_list[[indicador]], df)
#     }
#   }
# 
#   # Write each data frame to a separate CSV file
#   for (df_name in names(df_list)) {
#     df <- df_list[[df_name]]
#     write.csv(df, file.path("outputs", paste0(df_name, ".csv")))
#   }

#     df <-
#       # Table of selected dataset ----
#     output$table <- renderTable({
#       datasetInput()
#     })
#
#     # Downloadable csv of selected dataset ----
#     output$downloadData <- downloadHandler(
#       filename = function() {
#         paste(input$dataset, ".csv", sep = "")
#       },
#       content = function(file) {
#         write.csv(datasetInput(), file, row.names = FALSE)
#       }
#     )
# }
# - múltiplos indicadores -> resultados numa lista
# - teste geo usar sempre a Mealhada
# - simplificar listas geográficas
# - nomes das nuts 2002, eventualmente nuts 2023
# - simplificar ainda mais função (desnecessário aspas no x, permitir outros intervalos que não anual)
# - nomes automáticos nas variáveis
# - como resolver Campo de Ourique? (freguesia dividida em 2 ACES)

# Now i want the main panel below Real Data to have a reactive tabset with the itens in the results_list, then i want to renderDataTable that is in those itens and a download button that downloads a csv with the data