# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
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
library(readr)
library(jsonlite)
library(DT)
library(shinyjs)
# Load a reference table of geographical aggregations and Portuguese health clusters
geo_lookup <-
  read_csv(
    "datasets/geolinkage_aces_2022.csv",
    col_types = cols(.default = "c"),
    locale = locale("pt")
  )
# Remove `unknown` and `abroad` from the reference table
geo_lookup <- geo_lookup |>
  filter(!dicofre_2013 %in% c("0", "999999"))
# What data should be bound in the end?
geo_reference <- list(
  freguesia_2013 = geo_lookup,
  municipio_2013 = geo_lookup[3:20],
  municipio_2002 = geo_lookup[3:20],
  nuts3_2013 = geo_lookup[c(8:20)],
  nuts3_2002 = geo_lookup[c(8:20)],
  nuts2_2013 = geo_lookup[c(11:20)],
  nuts1_2013 = geo_lookup[c(13:20)],
  pais = geo_lookup[c(15:20)],
  aces_2022 = geo_lookup[c(3:5, 8:20)],
  ars_2022 = geo_lookup[c(3:5, 11:20)]
)
# Retrieve the available indicators
indicators <- read_excel("datasets/Indicadores.xlsx",
  # col_types = cols(.default = "c"),
  skip = 14
) |>
  clean_names() |>
  filter(disponivel_no_portal == "Sim") |>
  distinct(designacao, .keep_all = TRUE)
# Prepare an empty list for the results
result_list <- list()
meta_list <- list()
# Sleep function to prevent server lockout, rests for 5 seconds every 100 requests
sleep <- function(z) {
  if (z %% 100 == 0) {
    Sys.sleep(5)
    z <- 1
  } else {
    z <- z + 1
  }
  return(z)
}

ine.meta <- function(indicators, meta_list){
  counter <- 0
  for (i in 1:length(indicators)) {
    # Get the current indicator
    indicators_current <- indicators[i]
    # Call the sleep function
    counter <- sleep(counter)
    # Call the INE API to gather the datasets for each code
    results_raw <- fromJSON(
      paste0(
        "https://www.ine.pt/ine/json_indicador/pindicaMeta.jsp?varcd=",
        indicators_current,
        "&lang=PT"
      ))
    # Take the main features of the indicator - general for every indicator
    names <- results_raw %>% select(!c(Dimensoes,Sucesso))%>% pivot_longer(everything(), names_to = "Nome" , values_to = "Descricao")
    # Take the specific features of the indicator
    notas <- results_raw%>%
      unnest(Dimensoes)%>%
      select(Descricao_Dim)%>%
      unnest(Descricao_Dim)%>%
      rename("Nome" = abrv , "Descricao" = versao)%>%
      mutate(nome_dimensao= case_when(
        dim_num == 1 ~ "obs" ,
        dim_num == 2 ~ "geodsg" ,
        dim_num == 3 ~ "dim_3" ,
        dim_num == 4 ~ "dim_4" ,
        dim_num == 5 ~ "dim_5" ,
        dim_num == 6 ~ "dim_6" ,
        dim_num == 7 ~ "dim_7" ,
        dim_num == 8 ~ "dim_8" ,
        .default = NA
       ))%>%
      select(!dim_num)
    # check if column 'c' exists before mutating
    if (exists('nota_dsg',  notas)) {
      notas<-  notas %>% mutate(notadsg = nota_dsg)%>%select(!nota_dsg)
    }
    # Join both elements
    final_result <- bind_rows(names,notas)
    
    meta_list[[indicators_current]]<- final_result
  }
  return(meta_list)
}
# Main funtion for INE indicators extraction
ine.get <-
  function(indicators,
           selected_areas,
           observation_requested,
           result_list,
           geo_reference,
           groups_chosen,
           groups_other) {
    # Set the sleep timer to 0
    counter <- 0
    # Save the selected areas' codes into vectors
    dicofre_2013 <- unique(selected_areas$dicofre_2013)
    municipio_2013 <- unique(selected_areas$municipio_2013_cod)
    municipio_2002 <- unique(selected_areas$municipio_2002_cod)
    nuts_3_2013 <- unique(selected_areas$nuts3_2013_cod)
    nuts_3_2002 <- unique(selected_areas$nuts3_2002_cod)
    nuts_2_2013 <- unique(selected_areas$nuts2_2013_cod)
    nuts_1_2013 <- unique(selected_areas$nuts1_2013_cod)
    pais <- unique(selected_areas$pais_cod)
    # Joins the code vectors in a list
    codes_reference <- list(
      dicofre_2013,
      municipio_2013,
      municipio_2002,
      nuts_3_2013,
      nuts_3_2002,
      nuts_2_2013,
      nuts_1_2013,
      pais,
      ""
    )
    # Joins the dimension strings in a list
    dimmension_reference <- c(
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      "&Dim2=",
      ""
    )
    # Joins the col_names strings in a list
    level_names_reference <- c(
      "dicofre_2013",
      "municipio_2013_cod",
      "municipio_2002_cod",
      "nuts3_2013_cod",
      "nuts3_2002_cod",
      "nuts2_2013_cod",
      "nuts1_2013_cod",
      "pais_cod",
      ""
    )
    # Set the codes to test - one parish/municipality/NUTS III that changed codes between 2002 and 2013
    level_test <- c(
      # Tests parishes
      "&Dim2=011102&lang=PT",
      # Tests 2013 municipalities
      "&Dim2=16E0111&lang=PT",
      # Tests 2002 municipalities
      "&Dim2=1610111&lang=PT",
      # Tests 2013 NUTS III
      "&Dim2=16E&lang=PT",
      # Tests 2002 NUTS III
      "&Dim2=161&lang=PT",
      # Tests NUTS II
      "&Dim2=16&lang=PT",
      # Tests NUTS II
      "&Dim2=1&lang=PT",
      # Tests country
      "&Dim2=PT&lang=PT",
      # Sets no geographic specification, retrieves all available data
      "&lang=PT"
    )
    for (i in 1:length(indicators)) {
      # Get the current indicator
      indicators_current <- indicators[i]
      # Call the sleep function
      counter <- sleep(counter)
      # Prepare an empty list for the test
      test <- list()
      # Call the INE API to test the different codes and stores the results in a list of tibbles
      for (j in 1:length(level_test)) {
        counter <- sleep(counter)
        test[[j]] <-
          as.data.frame(fromJSON(
            paste0(
              "https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",
              indicators_current,
              "&Dim1=T",
              level_test[j]
            )
          ))
      }
      if (!is.null(groups_chosen) & !is.null(groups_other)) {
        groups_chosen <- c(groups_chosen, groups_other)
      }
      success <- c(10)
      for (k in 1:length(groups_chosen)) {
        # Set starting level
        l <- case_when(
          groups_chosen[k] == "Freguesia" ~ 1,
          groups_chosen[k] == "Município" ~ 2,
          groups_chosen[k] == "Distrito" ~ 2,
          groups_chosen[k] == "NUTS III" ~ 4,
          groups_chosen[k] == "NUTS II" ~ 6,
          groups_chosen[k] == "NUTS I" ~ 7,
          groups_chosen[k] == "País" ~ 8,
          groups_chosen[k] == "ACES" ~ 1,
          groups_chosen[k] == "ARS" ~ 2,
          # If all others fail, the default is the parish level
          TRUE ~ 1
        )
        # Sets the chosen values from the starting level
        codes_chosen <- codes_reference[[l]]
        dimmension_chosen <- dimmension_reference[l]
        geo_chosen <- geo_reference[[l]]
        level_names_chosen <- level_names_reference[l]
        # If it fails, then it increments until it finds a level with data
        while ("Falso" %in% colnames(test[[l]]$Sucesso) & l < 11) {
          l <- l + 1
          codes_chosen <- codes_reference[[l]]
          dimmension_chosen <- dimmension_reference[l]
          geo_chosen <- geo_reference[[l]]
          level_names_chosen <- level_names_reference[l]
          # Error condition when none of the selected levels have data
          if (l == 10) {
            errorCondition("Condições selecionadas sem resultados para este indicador.")
          }
        }
        # If the level we want was already retrieved, jump to the next `groups_chosen`
        if (l %in% success) {
          next
        } else {
          # Set an empty result data frame for all codes in each level
          df_all <- data.frame()
          # It increments along the codes
          for (m in 1:length(codes_chosen)) {
            # Call the sleep function
            counter <- sleep(counter)
            # Call the INE API to gather the datasets for each code
            results_raw <-
              fromJSON(
                paste0(
                  "https://www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=",
                  indicators_current,
                  "&Dim1=T",
                  dimmension_chosen,
                  codes_chosen[m],
                  "&lang=PT"
                )
              )
            # Extracts the data from  the INE API response
            results <- as.data.frame(results_raw$Dados)
            observation_available_names <- colnames(results)
            observation_available <- length(observation_available_names)
            # Checks if the requested number of observations is available
            if (observation_requested > observation_available) {
              observation_used <- observation_available
            } else {
              observation_used <- observation_requested
            }
            # Get names of the observations of interest
            observation_used_names <- c(observation_available_names[(observation_available - observation_used + 1):(observation_available)])
            # Set an empty result data frame for all observations in each code
            df_observations <- data.frame()
            # Loop over observations
            for (observation_current in observation_used_names) {
              # Remove everything except the observations we want
              df <- results |>
                unnest(!!sym(observation_current)) |>
                select(-any_of(observation_available_names)) |>
                mutate(
                  obs = as.character(observation_current),
                  valor = as.numeric(valor)
                )
              # Add results to data frame for all observations in each code
              df_observations <- bind_rows(df_observations, df)
            }
          # Add results to data frame for all codes in each level
          df_all <- bind_rows(df_observations, df_all)
          }
          # If only one level and code were requested, it outputs the results directly
          if (k == 1 & m == 1) {
            result_list[[indicators_current]] <- df_all
          } 
          if (groups_chosen[k] == "Distrito" & min(success) < 4) {
              # Adds the geographical information
              df_all <- df_all |>
                left_join(geo_chosen,
                  by = c("geocod" = as.character(level_names_chosen)),
                  multiple = "first"
                ) |>
                # Groups the results
                summarise(
                  geocod = distrito_2013_cod,
                  geodsg = distrito_2013,
                  valor = sum(valor),
                  obs = obs,
                  .by = c(obs, distrito_2013)
                ) |>
                select(geocod, geodsg, valor, obs)
            } else if (groups_chosen[k] == "ACES" & min(success) < 2) {
              # Adds the geographical information
              print(df_all)
              df_all <- df_all |>
                full_join(geo_chosen,
                  by = c("geocod" = as.character(level_names_chosen)),
                  multiple = "first"
                ) |>
                # Groups the results
                summarise(
                  geocod = aces_2022_cod,
                  geodsg = aces_2022,
                  valor = sum(valor),
                  obs = obs,
                  .by = c(obs, distrito_2013)
                ) |>
                select(geocod, geodsg, valor, obs)
            } else if (groups_chosen[k] == "ARS" & min(success) < 4) {
              # Adds the geographical information
              df_all <- df_all |>
                left_join(geo_chosen,
                  by = c("geocod" = as.character(level_names_chosen)),
                  multiple = "first"
                ) |>
                # Groups the results
                summarise(
                  geocod = ars_2022_cod,
                  geodsg = ars_2022,
                  valor = sum(valor),
                  obs = obs,
                  .by = c(obs, distrito_2013)
                ) |>
                select(geocod, geodsg, valor, obs)
            } else {
            result_list[[indicators_current]] <-
              bind_rows(result_list[[indicators_current]], df_all)|>unique() }
            success <- sort(c(success, l))
          }
        }
      }
    return(result_list)
  }
#
chosen_group_options <- NULL
# Define UI for application that draws a histogram
ui <- navbarPage(
  "Extrator INE v0.2.1",
  tabPanel(
    "Extração de dados",
    useShinyjs(),
    fluidRow(
      column(
        width = 2,
        align = "right",
        height = 60,
        imageOutput("sns_img1", height = "60px")
      ),
      column(
        width = 2,
        align = "right",
        height = 60,
        imageOutput("dgs_img1", height = "60px")
      ),
      column(
        width = 3,
        align = "right",
        height = 60,
        imageOutput("ine_img1", height = "60px")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        br(),
        sliderInput(
          "observation_slider",
          "Número de observações a pedir:",
          min = 1,
          max = 30,
          value = 1
        ),
        br(),
        p("Se o número de observações pedidas não estiver disponível, será extraído o máximo possível."),
        br(),
        # Search bar for indicator
        selectizeInput(
          "indicators_search",
          "Selecionar ou Pesquisar indicadores:",
          choices = NULL,
          multiple = TRUE
        ),
        p("Lista de indicadores atualizada a 2023-03-06"),
        br(),
        # Search bar for desagregação
        selectInput(
          "chosen_group_dropdown",
          "Nível geográfico:",
          choices = c(
            "Freguesia",
            "Município",
            "Distrito",
            "NUTS III",
            "NUTS II",
            "NUTS I",
            "País",
            "ACES",
            "ARS"
          )
        ),
        p(
          "Se o nível pedido não estiver disponível, será extraído o mais próximo possível."
        ),
        br(),
        # Dropdown for additional desagregação options
        uiOutput("chosen_items_search"),
        checkboxInput("other_groups_checkbox",
                      "Agrupar resultados por outros níveis",
                      FALSE),
        uiOutput("other_groups_search"),
        checkboxInput("meta_checkbox",
                      "Pedir Metainformação",
                      FALSE),
        actionButton("go", "Submeter", class = "btn-primary"),
        checkboxInput(
          "show_debug",
          "Painel debug",
          FALSE
        )
      ),
      # Show a plot of the generated distribution
      mainPanel(
        uiOutput("debug_panel_checkbox"),
        h2("Dados Recolhidos pelo Extractor"),
        uiOutput("error"),
        uiOutput("results_table")
      )
    )
  ),
  tabPanel(
    "Sobre",
    fluidRow(
      column(
        width = 2,
        align = "right",
        height = 60,
        imageOutput("sns_img2", height = "60px")
      ),
      column(
        width = 2,
        align = "right",
        height = 60,
        imageOutput("dgs_img2", height = "60px")
      ),
      column(
        width = 3,
        align = "right",
        height = 60,
        imageOutput("ine_img2", height = "60px")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        h3(strong("Autoria")),
        h4(
          strong("João Dionísio, Rafael Vasconcelos")
        ),
        br()
      ),
      mainPanel(
        h3("Próximas melhorias"),
        p("- Corrigir o cálculo dos indicadores para distrito, ACES, ARS;"),
        p("- Automatizar a procura dos indicadores disponíveis;"),
        p("- Permitir a manipulação de variáveis;"),
        p("- Comentar o código."),
        h2("Changelog"),
        h3("V0.2.2"),
        h4("2023-03-16"),
        p("- Possibilitada a transferência de metadados."),
        p("- Alteração do ficheiro de saída com UTF-8 para manter caracteres especiais."),
        br(),
        h3("V0.2.1"),
        h4("2023-03-16"),
        p("- Remoção temporária de opções de outros níveis que causavam erro."),
        p("- Redução no número de chamadas ao servidor, se houver redundâncias."),
        br(),
        h3("V0.2.0"),
        h4("2023-03-15"),
        p("- Reescrita das instruções;"),
        p("- Estruturação dos resultados com várias níveis geográficos em simultâneo."),
        br(),
        h3("V0.1.1"),
        h4("2023-03-14"),
        p("- Redução do tempo entre chamadas ao servidor;"),
        p("- Otimização do número de uniões dos resultados recebidos;"),
        p("- Correção de erro na listagem dos municípios.")
      )
    )
  )
)
#
server <- function(input, output, session) {
  output$sns_img1 <- renderImage(
    {
      list(
        src = "www/SNS.png",
        height = 60
      )
    },
    deleteFile = F
  )
  output$dgs_img1 <- renderImage(
    {
      list(
        src = "www/DGS.png",
        height = 60
      )
    },
    deleteFile = F
  )
  # Renders the logos to be called in the tabs
  output$ine_img1 <- renderImage(
    {
      list(
        src = "www/INE.gif",
        height = 60
      )
    },
    deleteFile = F
  )
  output$sns_img2 <- renderImage(
    {
      list(
        src = "www/SNS.png",
        height = 60
      )
    },
    deleteFile = F
  )
  output$dgs_img2 <- renderImage(
    {
      list(
        src = "www/DGS.png",
        height = 60
      )
    },
    deleteFile = F
  )
  output$ine_img2 <- renderImage(
    {
      list(
        src = "www/INE.gif",
        height = 60
      )
    },
    deleteFile = F
  )
  # Update the choices for the indicators search based on the text input
  updateSelectizeInput(
    session,
    "indicators_search",
    choices = indicators$designacao,
    options = list(
      placeholder = "Barra de Pesquisa",
      create = FALSE,
      maxOptions = 15
    ),
    server = TRUE
  )
  # Retrieves the list of available items for the chosen geographic level
  chosen_group_options <- reactive({
    available_items <- NULL
    if (input$chosen_group_dropdown == "Freguesia") {
      available_items <- geo_lookup$freguesia_2013
    } else if (input$chosen_group_dropdown == "Município") {
      available_items <- geo_lookup$municipio_2013
    } else if (input$chosen_group_dropdown == "Distrito") {
      available_items <- geo_lookup$distrito_2013
    } else if (input$chosen_group_dropdown == "NUTS III") {
      available_items <- geo_lookup$nuts3_2013
    } else if (input$chosen_group_dropdown == "NUTS II") {
      available_items <- geo_lookup$nuts2_2013
    } else if (input$chosen_group_dropdown == "NUTS I") {
      available_items <- geo_lookup$nuts1_2013
    } else if (input$chosen_group_dropdown == "País") {
      available_items <- geo_lookup$pais
    } else if (input$chosen_group_dropdown == "ACES") {
      available_items <- geo_lookup$aces_2022
    } else if (input$chosen_group_dropdown == "ARS") {
      available_items <- geo_lookup$ars_2022
    }
    return(available_items)
  })
  # Creates the dynamic dropdown menu with the available items for the chosen geographic level
  output$chosen_items_search <- renderUI({
    if (!is.null(chosen_group_options())) {
      selectizeInput(
        "chosen_items",
        "Selecionar itens a incluir:",
        # choices = NULL,
        choices = chosen_group_options(),
        multiple = TRUE,
        options = list(
          placeholder = "Barra de pesquisa",
          create = FALSE
        )
      )
    }
  })
  # Creates a debug panel with the codes in the current selection
  output$debug_panel_checkbox <- renderUI({
    if (input$show_debug == TRUE) {
      tagList(
        h4("Painel Debug"),
        tabsetPanel(
          tabPanel(
            "Código do Indicador",
            verbatimTextOutput("filtered_indicators")
          ),
          tabPanel(
            "Códigos Freguesias",
            verbatimTextOutput("filtered_freguesia")
          ),
          tabPanel(
            "Códigos Municípios 2013",
            verbatimTextOutput("filtered_municipio_2013")
          ),
          tabPanel(
            "Códigos Municípios 2002",
            verbatimTextOutput("filtered_municipio_2002")
          ),
          tabPanel("Nível Filtrado", verbatimTextOutput("groups_chosen")),
          tabPanel(
            "Outros Níveis a Agrupar",
            verbatimTextOutput("groups_other")
          ),
          tabPanel("Tabela de Freguesias", DT::dataTableOutput("df"))
        )
      )
    } else {
      NULL
    }
  })
  # Create a reactive function for the other_groups_list options
  other_groups_options <- reactive({
    other_groups <- NULL
    if (input$chosen_group_dropdown == "Freguesia") {
      other_groups <-
        c(
          "Município",
          # "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País"
        )
    } else if (input$chosen_group_dropdown == "Município") {
      other_groups <-
        c(
          "Freguesia",
          # "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País"
        )
    } else if (input$chosen_group_dropdown == "Distrito") {
      other_groups <-
        c(
          "Freguesia",
          "Município" # ,
          # "NUTS III",
          # "NUTS II",
          # "NUTS I",
          # "País"
        )
    } else if (input$chosen_group_dropdown == "NUTS III") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          # "Distrito",
          "NUTS II",
          "NUTS I",
          "País"
        )
    } else if (input$chosen_group_dropdown == "NUTS II") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          # "Distrito",
          "NUTS III",
          "NUTS I",
          "País"
        )
    } else if (input$chosen_group_dropdown == "NUTS I") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          "Distrito",
          "NUTS III",
          "NUTS II",
          "País"
        )
    } else if (input$chosen_group_dropdown == "País") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I"
        )
    } else if (input$chosen_group_dropdown == "ACES") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          # "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País" # ,
          # "ARS"
        )
    } else if (input$chosen_group_dropdown == "ARS") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          # "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País",
          "ACES"
        )
    }
    return(other_groups)
  })
  #
  output$other_groups_search <- renderUI({
    if (!is.null(chosen_group_options()) & input$other_groups_checkbox == TRUE) {
      selectizeInput(
        "other_groups_list",
        "Selecionar níveis a incluir:",
        choices = other_groups_options(),
        multiple = TRUE,
        options = list(
          placeholder = "Barra de pesquisa",
          create = FALSE
        )
      )
    }
  })
  # Create a reactive function for the focused area codes
  filtered_area <- reactive({
    filtered <-
      geo_lookup |> filter(chosen_group_options() %in% input$chosen_items)
    # Sets lists of codes for debug panel
    f_freguesia <- filtered |>
      pull(dicofre_2013) |>
      unique()
    f_municipio_2013 <- filtered |>
      pull(municipio_2013_cod) |>
      unique()
    f_municipio_2002 <- filtered |>
      pull(municipio_2002_cod) |>
      unique()
    return(
      list(
        filtered_table = filtered,
        f_freguesia = f_freguesia,
        f_municipio_2013 = f_municipio_2013,
        f_municipio_2002 = f_municipio_2002
      )
    )
  })
  # Create a reactive function to ge the chosen indicator codes
  filtered_indicators <- reactive({
    f_indicators <- indicators |>
      filter(designacao %in% input$indicators_search) |>
      pull(codigo_de_difusao)
    return(f_indicators)
  })
  # Output the filtered dataset to the "df" table
  output$df <- DT::renderDataTable({
    filtered_area()$filtered_table
  })
  # Output the filtered codigo_de_difusao to "filtered_dataset"
  output$filtered_indicators <- renderPrint({
    filtered_indicators()
  })
  output$filtered_freguesia <- renderPrint({
    filtered_area()$f_freguesia
  })
  output$filtered_municipio_2013 <- renderPrint({
    filtered_area()$f_municipio_2013
  })
  output$filtered_municipio_2002 <- renderPrint({
    filtered_area()$f_municipio_2002
  })
  output$groups_chosen <- renderPrint({
    input$chosen_group_dropdown
  })
  output$groups_other <- renderPrint({
    input$other_groups_list
  })
  #
  result_list_reactive <- reactiveVal()
  meta_list_reactive <- reactiveVal()
  #
  dimmension_chosen <- reactiveVal()
  #
  observeEvent(input$go, {
    # Disable inputs
    shinyjs::disable(selector = "input")
    shinyjs::disable(selector = "select")
    shinyjs::disable(selector = "button")
    result_list <- result_list
    meta_list <- meta_list
    # Extract data from INE using the inputs from the UI
    if (length(filtered_indicators()) != 0 &
      nrow(filtered_area()$filtered_table) != 0) {
      result_list_updated <- ine.get(
        indicators = filtered_indicators(),
        selected_areas = filtered_area()$filtered_table,
        observation_requested = input$observation_slider,
        result_list = result_list,
        geo_reference = geo_reference,
        groups_chosen = input$chosen_group_dropdown,
        groups_other = input$other_groups_list
      )
      result_list_reactive(result_list_updated)
      dimmension_chosen(c(input$other_groups_list, input$chosen_group_dropdown))
      output$error <- NULL
      if(input$meta_checkbox == TRUE){
        meta_list_updated <- ine.meta(
          indicators = filtered_indicators(),
          meta_list = meta_list
        )
        meta_list_reactive(meta_list_updated)
      }else{
        next
      }
    } else if (length(filtered_indicators()) == 0) {
      output$error <- renderUI({
        tagList(
          br(),
          h1(strong("Não foi pedido nenhum indicador")),
          br()
        )
      })
    } else {
      output$error <- renderUI({
        tagList(
          br(),
          h1(strong(
            "Não foi pedida nenhuma desagregação"
          )),
          br()
        )
      })
    }
    # Enable inputs
    shinyjs::enable(selector = "input")
    shinyjs::enable(selector = "select")
    shinyjs::enable(selector = "button")
  })
  # Render the tabs based on the reactive value
  output$results_table <- renderUI({
    # Get the items from result_list_reactive
    items <- names(result_list_reactive())
    # Create a list of tabPanels with dataTables and downloadButtons
    tabs <- lapply(items, function(item) {
      full_name <- indicators$designacao[indicators$codigo_de_difusao == item]
      title <-
        substr(indicators$designacao[indicators$codigo_de_difusao == item], 1, 20)
      if (length(full_name) == 0) {
        full_name <- item
      } else {
        full_name <- full_name[1]
      }
      tabPanel(
        title,
        # set tooltip with full name
        h4(strong(full_name)),
        DT::dataTableOutput(paste0(item, "_table")),
        downloadButton(paste0(item, "_download"), paste0(item, ".csv")),
        if(input$meta_checkbox == TRUE){
          downloadButton(paste0(item,"meta", "_download"), paste0(item,"meta",".csv"))
        }else{
          next
        }
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
          fwrite(data, file,sep = ";", bom = TRUE)
        }
      )
    if(input$meta_checkbox == TRUE){ 
      output[[paste0(item,"meta","_download")]] <- downloadHandler(
        filename = function() {
          paste0(item,"meta",".csv")
        },
        content = function(file) {
          # Get the data from result_list_reactive
          data <- meta_list_reactive()[[item]]
          # Write the data to a csv file
          fwrite(data, file, sep = ";", bom = TRUE)
        }
      )}else{
        next
      }
    })
  })
}


# Run the application
shinyApp(ui, server)
