#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Extração de Dados do INE"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs_back",
                        "Number of bins:",
                        min = 2,
                        max = 30,
                        value = 2),
            selectInput("dataset", "Choose a dataset:",
                        choices = Indicadores$Designação),

        
        ),
        # Show a plot of the generated distribution
        mainPanel(
          
          tableOutput("df"),
          # Button
          downloadButton("downloadData", "Download")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  linkage_geo <- read_csv("datasets/linkage_geo.csv")
  
  linkage_geo <- linkage_geo %>%
    select(geo, ars, nuts3, aces2, nuts1, nuts2, cod_geo_nuts2002, cod_geo)  %>%
    rename(municip="geo")%>%
    mutate(municip= recode(municip, "Calheta"="Calheta [R.A. Madeira]"))
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  selected_row <- Indicadores[Indicadores$Designação == input$dataset,]
  indicador <- selected_row$`Código de difusão`
  
  #DEFINIR INDICADORES A RETIRAR
  
  # indicador <- c("0008614","0009817") 
  
  #TERMO DE DESAGREGACAO POR LOCALIZACAO GEOGRAFICA
  desag <- "&Dim2="
  
  #CODIGOS GEOGRAFICOS NUTSIII PARA 2012 e 2002
  cod_2002 <-linkage_geo$cod_geo_nuts2002
  cod_2002 <- cod_2002[! cod_2002 %in% c("9999999", "0")]
  cod_2014 <- linkage_geo$geocod
  cod_2014 <- cod_2014[! cod_2014 %in% c("9999999", "0")]
  
  #CODIGOS PARA NUTSII
  nuts_ii_cod <- c("PT","11","16","17","18","19","20","30")
  
  #DEFINIR ANOS A RETIAR Minimo 2
  obs_back <- input$obs_back
  
  # DEFINICOES BASEADO NO COLOCADO EM CIMA
  
  a <- 1
  
  b <- length(cod_2014) 
  
  c <- length(nuts_ii_cod)
  
  #A API DO INE TEM UM LIMITE DE REQUESTS LOGO E CRIADO UM VALOR QUE VAI AUMENTANDO COM AS REQUESTS QUE SAO FEITAS
  
  counter <- 1
  
  #LIMPA A ESTRACAO ANTERIOR
  result_list <- list()
  
  #FUNCAO DE SLEEP - funciona se sleep e 60, 30, 20, 10, 
  
  sleep <- function(z){
    if (z %% 100 == 0) {
      Sys.sleep(5)
      z <- 1
    }else{
      z <- z+1
    }
  }
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
      if (w %% 100 == 0) {
        print(cod_2014[w])
      }
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
        if (e %% 100 == 0) {
          print(cod_2014[e])
        }
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
  file_names <- list.files(path = "outputs", pattern = ".*\\.csv")
  
  # Initialize a list to store the data frames
  df_list <- list()
  #FOR LOOP
  for (file_name in file_names) {
    # Split the file name into its components
    components <- strsplit(file_name, c("_"))[[1]]
    
    # Remove the ".csv" extension from the last component
    last_component <- sub(".csv", "", components[length(components)])
    components[length(components)]
    # Replace the last component in the components vector
    components[length(components)] <- last_component
    # Assign the components to variables
    indicador <- components[1]
    year <- components[2]
    aggregation <- components[3]
    
    # Load the file into a data frame
    df <- read.csv(file.path("outputs", file_name))
    
    # Add columns for year and aggregation
    df <- cbind(df, year = year, aggregation = aggregation)
    
    # Check if a data frame for this indicator already exists
    if (!(indicador %in% names(df_list))) {
      # If not, create a new data frame for this indicator
      df_list[[indicador]] <- df
    } else {
      # If it does, use rbind to combine the data frames
      df_list[[indicador]] <- rbind(df_list[[indicador]], df)
    }
  }
  
  # Write each data frame to a separate CSV file
  for (df_name in names(df_list)) {
    df <- df_list[[df_name]]
    write.csv(df, file.path("outputs", paste0(df_name, ".csv")))
  }

    df <- 
      # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
