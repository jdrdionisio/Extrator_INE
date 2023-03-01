#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shinyWidgets")
# install.packages("fuzzywuzzyR")
# install.packages("DT")
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(janitor)
library(DT)

Indicadores <- 
  read_excel("datasets/Indicadores.xlsx", 
             skip = 14)%>%
  clean_names()%>%
  filter(disponivel_no_portal== "Sim")
      
linkage_geo <- read_csv("datasets/linkage_geo.csv", show_col_types = FALSE)%>%
  select(geo, ars, nuts3, aces2, nuts1, nuts2, cod_geo_nuts2002, cod_geo)  %>%
  rename(municip="geo")%>%
  mutate(municip= recode(municip, "Calheta"="Calheta [R.A. Madeira]"))

nuts_ii_cod <- c("PT","11","16","17","18","19","20","30")
c <- length(nuts_ii_cod)
counter <- 1
result_list <- list()

sleep <- function(z){
  if (z %% 100 == 0) {
    Sys.sleep(5)
    z <- 1
  }else{
    z <- z+1
  }}

extract_data <- function(indicador, desag, cod_2014, obs_back, a, b) {
  result_list <- list()
  counter <- 0
  for (k in 1:a) {
    #COMECA POR LER O INDICADOR A RETIRAR
    indicador_atual <- indicador[k]
    counter <- sleep(counter)
    #TESTA A DESAGREGAGACAO DE 2014
    result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T", desag, cod_2014[79], "&lang=PT"))
    result <- as.data.frame(result)
    #OS DADOS DO INE VEM COM MI E DADOS - PARA A EXTRACAO
    if (("IndicadorCod" %in% colnames(result))){
      for (w in 1:b) {
        #retirar indicador com desagregação municipio 2014
        counter <- sleep(counter)
        result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T", desag, cod_2014[w], "&lang=PT"))
        agreg <- "NUTSIII"
        dados <- as.data.frame(result$Dados)
        colunas_nanos <- colnames(dados)
        num_colunas <- length(colunas_nanos)
        if(obs_back > num_colunas){
          x <- num_colunas
        }else{
          x <-  obs_back
        }
        obs_backs <- as.character(c(colunas_nanos[(num_colunas-x):(num_colunas)]))
        for (i in 1:x) {
          #ver e retirar os nomes das colunas de cada ano para o select
          obs_backs2 <-  obs_backs[-i]
          dados1 <- dados %>%
            unnest(obs_backs[i])%>%
            select(-all_of(obs_backs2))
          result_list[[obs_backs[i]]] <- bind_rows(result_list[[obs_backs[i]]], dados1)
          Sys.sleep(1)
        }}
    }
    else if(length(result_list)==0){
      counter <- sleep(counter)
      #TESTA A DESAGREGAGACAO DE 2002
      result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T",desag, cod_2002[1], "&lang=PT"))
      result <- as.data.frame(result)
      if (("IndicadorCod" %in% colnames(result))) {
        #retirar indicador com desagregação municipio 2002
        for (e in 1:b) {
          counter <- sleep(counter)
          result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T",desag, cod_2002[e], "&lang=PT"))
          agreg <- "NUTSIII"
          dados <- as.data.frame(result$Dados)
          colunas_nanos <- colnames(dados)
          num_colunas <- length(colunas_nanos)
          if(obs_back > num_colunas){
            x <- num_colunas
          }else{
            x <-  obs_back
          }
          obs_backs <- as.character(c(colunas_nanos[(num_colunas-x):(num_colunas)]))
          for (i in 1:x) {
            #ver e retirar os nomes das colunas de cada ano para o select
            obs_backs2 <-  obs_backs[-i]
            dados1 <- dados %>%
              unnest(obs_backs[i])%>%
              select(-all_of(obs_backs2))
            result_list[[obs_backs[i]]] <- bind_rows(result_list[[obs_backs[i]]], dados1)
            Sys.sleep(1)
          }}
      }
    }
    else if(length(result_list)==0){
      counter <- sleep(counter)
      #TESTA A DESAGREGAGACAO DE NUTSII
      result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T",desag, nuts_ii_cod[1], "&lang=PT"))
      result <- as.data.frame(result)
      if (("IndicadorCod" %in% colnames(result))) {
        #retirar indicador com desagregação NUTSII
        for (r in 1:c) {
          counter <- sleep(counter)
          result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual ,"&Dim1=T",desag, nuts_ii_cod[r], "&lang=PT"))
          agreg <- "NUTSII"
          dados <- as.data.frame(result$Dados)
          colunas_nanos <- colnames(dados)
          num_colunas <- length(colunas_nanos)
          if(obs_back > num_colunas){
            x <- num_colunas
          }else{
            x <-  obs_back
          }
          obs_backs <- as.character(c(colunas_nanos[(num_colunas-x):(num_colunas)]))
          for (i in 1:x) {
            #ver e retirar os nomes das colunas de cada ano para o select
            obs_backs2 <-  obs_backs[-i]
            dados1 <- dados %>%
              unnest(obs_backs[i])%>%
              select(-any_of(obs_backs2))
            result_list[[obs_backs[i]]] <- bind_rows(result_list[[obs_backs[i]]], dados1)
            Sys.sleep(1)
          }}
      }
    }
    else{
      counter <- sleep(counter)
      #TESTA A DESAGREGAGACAO NACIONAL OU FORA DO PADRAO
      result <- fromJSON(paste0("https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",indicador_atual,"&Dim1=T&lang=PT"))
      if (("IndicadorCod" %in% colnames(result))) {
        #retirar indicador sem desagragação
        agreg <- "nacional"
        result <- as.data.frame(result)
        dados <- as.data.frame(result$Dados)
        colunas_nanos <- colnames(dados)
        num_colunas <- length(colunas_nanos)
        if(obs_back > num_colunas){
          x <- num_colunas
        }else{
          x <-  obs_back
        }
        obs_backs <- as.character(c(colunas_nanos[(num_colunas-x):(num_colunas)]))
        for (i in 1:x) {
          #ver e retirar os nomes das colunas de cada ano para o select
          obs_backs2 <-  obs_backs[-i]
          dados1 <- dados %>%
            unnest(obs_backs[i])%>%
            select(-all_of(obs_backs2))
          result_list[[obs_backs[i]]] <- bind_rows(result_list[[obs_backs[i]]], dados1)
          Sys.sleep(1)
        }}
    }
    if (length(result_list)!=0){
      x <- length(result_list)
      for (i in 1:x) {
        nomedf <- paste0(indicador_atual,"_",obs_backs[i],"_",agreg)
        assign(paste0("full_df"), as.data.frame(result_list[[obs_backs[i]]])%>%
                 select(-any_of(colunas_nanos))%>%left_join(linkage_geo, by="geocod"))
        fwrite(full_df, file = paste0("outputs/",nomedf,".csv"), bom = T)
      }
    }
  }


  return(result_list)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(
    h1("Extração de Dados do INE", align= "center")
    ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3(strong("Autoria: João Dionísio e Rafael Vasconcelos")),
      br(),
      sliderInput("obs_back",
                  "Número de observações:",
                  min = 2,
                  max = 30,
                  value = 2),
      # Search bar for dataset
      selectizeInput("dataset",
                     "Selecionar um indicador:",
                     choices = Indicadores$designacao,
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Search for datasets...',
                       create = FALSE,
                       maxOptions = 5)
                     ),
      # Search bar for desagregação
      selectInput("desagregacao",
                  "Selecionar a desagregação:",
                  choices = c("ACES", "ARS", "Nacional", "Concelho", "Freguesia"),
                  selected = "Nacional"),
      
      # Dropdown for additional desagregação options
      uiOutput("desagregacao_opcoes"),
      actionButton("go", "Submeter")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Debug Panel"),
      tabsetPanel(
        tabPanel("Tabela", DT::dataTableOutput("df")),
        tabPanel("Número de Indicador", verbatimTextOutput("filtered_ind")),
        tabPanel("Códigos 2002", verbatimTextOutput("filtered_ind1")),
        tabPanel("Códigos 2014", verbatimTextOutput("filtered_ind2")),
      ),
      h1("Real Data"),
      # Button
      downloadButton("downloadData", "Download")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Create a reactive function for the desagregacao options
  desag_opcoes <- reactive({
    opcoes <- NULL
    
    if(input$desagregacao == "ACES") {
      opcoes <- linkage_geo$aces2
    } else if(input$desagregacao == "ARS") {
      opcoes <- linkage_geo$ars
    } else if(input$desagregacao == "Concelho") {
      opcoes <- linkage_geo$municip
    } else if(input$desagregacao == "Freguesia") {
      opcoes <- linkage_geo$freguesia
    }
  
    return(opcoes)
  })
  
  # Create the dynamic dropdown menu for desagregacao_opcoes
  output$desagregacao_opcoes <- renderUI({
    if(!is.null(desag_opcoes())) {
      selectizeInput("area",
                     "Selecionar um filtro de área:",
                     choices = desag_opcoes(),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Search for datasets...',
                       create = FALSE)
      )
    }
  })
  # Create a reactive function for the filtered dataset
  filtered_desag <- reactive({
    if(!is.null(desag_opcoes())){
      filtered <- subset(linkage_geo, desag_opcoes() %in% input$area)}
    else{
      filtered <- linkage_geo
    }
    filtered1 <- filtered$cod_geo_nuts2002
    a <- 1
    b <- length(filtered1)
    filtered2 <- filtered$cod_geo
    return(list(filtered_table = filtered, filtered1 = filtered1, filtered2 = filtered2,a=a,b=b))
  })
  # Create a reactive function for the filtered dataset
  filtered_indicador <- reactive({
    filtered <- subset(Indicadores, designacao %in% input$dataset)
    filtered_ind <- filtered$codigo_de_difusao
    return(filtered_ind)
  })
  # Output the filtered dataset to the "df" table
  output$df <- DT::renderDataTable(
    {filtered_desag()$filtered_table}
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
  # Other server code goes here
  v <- reactiveValues(download= FALSE)
  
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$download <- input$go
  })
  # 
  # output$plot <- renderPlot({
  #   if (v$doPlot == FALSE) return()
  #   
  #   isolate({
  #     data <- if (input$tabset == "Uniform") {
  #       runif(input$unifCount, input$unifRange[1], input$unifRange[2])
  #     } else {
  #       rnorm(input$normCount, input$normMean, input$normSd)
  #     }
  #     
  #     hist(data)
  #   })
  # })
  
}

# Run the application 
shinyApp(ui, server)
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

