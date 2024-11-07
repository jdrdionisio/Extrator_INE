# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
library(shiny, quietly = T)
library(shinyWidgets, quietly = T)
library(tidyverse, quietly = T)
library(janitor, quietly = T)
library(readxl, quietly = T)
library(jsonlite, quietly = T) # changing to yyjsonr
library(data.table, quietly = T)
library(DT, quietly = T)
library(shinyjs, quietly = T)
library(bslib, quietly = T)
library(shinycssloaders, quietly = T)
library(ggthemes, quietly = T)
library(plotly, quietly = T)
library(yyjsonr, quietly = T)
library(httr2, quietly = T)
# Load a reference table of geographical aggregations and Portuguese health clusters
# geo_lookup <-
#   read_delim(
#     "datasets/geo_linkage_2024_v2.csv",
#     delim=";",
#     col_types = cols(.default = "c"),
#     locale = locale("pt")
#   )
geo_lookup <- fread("datasets/geo_linkage_2024_v2.csv")
# Remove `unknown` and `abroad` from the reference table
geo_lookup <- geo_lookup |>
  filter(!dicofre_2013 %in% c("0", "999999"))
# What data should be bound in the end?
geo_reference <- list(
  freguesia_2013 = geo_lookup,
  municipio_2013 = geo_lookup[3:22],
  municipio_2002 = geo_lookup[3:22],
  nuts3_2013 = geo_lookup[c(8:22)],
  nuts3_2002 = geo_lookup[c(8:22)],
  nuts2_2013 = geo_lookup[c(11:22)],
  nuts1_2013 = geo_lookup[c(13:22)],
  pais = geo_lookup[c(15:22)],
  aces_2022 = geo_lookup[c(3:5, 8:22)],
  uls_2023 = geo_lookup[c(3:5, 8:22)],
  ars_2022 = geo_lookup[c(3:5, 11:22)]
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
chosen_group_options <- NULL
# Define UI for application that draws a histogram
# thematic::thematic_shiny(font_google("Lato"))
# Sleep function to prevent server lockout, rests for 5 seconds every 100 requests
# sleep <- function(z) {
#   if (z %% 99 == 0) {
#     Sys.sleep(1)
#     z <- 1
#   } else {
#     z <- z + 1
#   }
#   return(z)
# }

unpack_df <- function(nested_df) {
  list_rbind(map2(nested_df, names(nested_df), ~ {
    if ("valor" %in% names(.x)) {
      .x$valor <- as.numeric(.x$valor)
    }
    .x$obs <- .y
    .x
  }))
}
apply_filters <- function(df, groups_to_exclude, codes_reference,groups_chosen) {
  filter_conditions <- list(
    "Município" = codes_reference[[2]],
    "NUTS III" = codes_reference[[4]],
    "NUTS II" = codes_reference[[6]],
    "NUTS I" = codes_reference[[7]],
    "País" = codes_reference[[8]]
  )
      for (group in groups_to_exclude) {
         if (!group %in% groups_chosen) {
             df <- df |> 
               filter(!geocod %in% filter_conditions[[group]])
         }
     }
   return(df)
}
# Function to generate dim column names
generate_dim_columns <- function(num_dims) {
  dim_columns <- c()
  for (i in seq_len(num_dims / 2)) {
    dim_columns <- c(dim_columns, paste0("dim_", i + 2), paste0("dim_", i + 2, "_t"))
  }
  return(dim_columns)
}

synthetic_level <- data.frame(
  Distrito=c("distrito_2013","distrito_2013_cod"),
  ACES=c("aces_2022","aces_2022_cod"),
  ARS=c("ars_2022","ars_2022_cod"),
  ULS=c("uls_2023","uls_2023_cod")
)

join_synthetic <- function(df, synthetic_group,level_names_success, geo_chosen){
  num_dims <- sum(str_detect(colnames(df), "dim"))
  col_dims <- generate_dim_columns(num_dims)
  geo_chosen <- geo_chosen |> 
    rename("geocod"={{level_names_success}})
  syn <- synthetic_level[[synthetic_group]]
  summarise_by <- c("obs", syn[1] , col_dims)
  select <- c("geocod", "geodsg",col_dims, "valor", "obs")
  new_df <- df |> 
    # Adds the geographical information
    left_join(geo_chosen,multiple = "first") |> 
    summarise(
      geocod = .data[[syn[2]]],
      geodsg = .data[[syn[1]]],
      valor = sum(valor,na.rm=TRUE),
      # obs = obs,
      .by = all_of(summarise_by))|>
    select(all_of(select)) |>
    mutate(
      geocod=as.character(geocod)
    ) |>
    # filter(!is.na(geocod)) |> 
    unique()
  # print(new_df)
  df <- bind_rows(df,new_df)
  return(df)
}
ine.meta <- function(indicators, meta_list){
  counter <- 0
  for (i in 1:length(indicators)) {
    # Get the current indicator
    indicators_current <- indicators[i]
    # Call the INE API to gather the datasets for each code
    test <- request(paste0(
      "https://www.ine.pt/ine/json_indicador/pindicaMeta.jsp?varcd=",
      indicators_current,
      "&lang=PT"
    )) |> 
      req_headers("Accept" = "application/json") |> 
      req_method("GET") |> 
      req_perform()
    results_raw <- test[["body"]] |> 
      read_json_raw()
    names <- results_raw |>  select(!c(Dimensoes,Sucesso)) |>  pivot_longer(everything(), names_to = "Nome" , values_to = "Descricao")
    notas <- results_raw$Dimensoes[[1]][["Descricao_Dim"]] |> 
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
    # check if column 'nota_dsg' exists before mutating
    if (exists('nota_dsg',  notas)) {
      notas <-  notas %>% mutate(notadsg = nota_dsg)%>%select(!nota_dsg)
    }
    final_result <- bind_rows(names,notas)
    meta_list[[indicators_current]]<- final_result
  }
  return(meta_list)
}
# Main funtion for INE indicators extraction
ine.get <- function(indicators,selected_areas,observation_requested, result_list, geo_reference,groups_chosen, groups_other, individual, all) {
    # Set the sleep timer to 0
    # counter <- 0
    # Save the selected areas' codes into vectors
    # Joins the code vectors in a list
    codes_reference <- list(
      dicofre_2013 <- unique(selected_areas$dicofre_2013),
      municipio_2013 <- unique(selected_areas$municipio_2013_cod),
      municipio_2002 <- unique(selected_areas$municipio_2002_cod),
      nuts_3_2013 <- unique(selected_areas$nuts3_2013_cod),
      nuts_3_2002 <- unique(selected_areas$nuts3_2002_cod),
      nuts_2_2013 <- unique(selected_areas$nuts2_2013_cod),
      nuts_1_2013 <- unique(selected_areas$nuts1_2013_cod),
      pais <- unique(selected_areas$pais_cod),
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
    level_test <- c(
      "011102",      # Tests parishes
      "16E0111",      # Tests 2013 municipalities
      "1610111",  # Tests 2002 municipalities
      "16E", # Tests 2013 NUTS III
      "161",  # Tests 2002 NUTS III
      "16",       # Tests NUTS II
      "1", # Tests NUTS II
      "PT" # Tests country
    )
    for (i in 1:length(indicators)) {
      # Get the current indicator
      indicators_current <- indicators[i]
      # Call the INE API to test the different codes and stores the results in a list of tibbles
      request_base <- request(
        "https://www.ine.pt/ine/json_indicador/pindica.jsp") 
      params <- list(op=2, varcd=indicators_current, Dim1="T",lang="PT")
      
      reqs <- list(
        request_base |> req_url_query(!!!params,Dim2=level_test[1]),
        request_base |> req_url_query(!!!params,Dim2=level_test[2]),
        request_base |> req_url_query(!!!params,Dim2=level_test[3]),
        request_base |> req_url_query(!!!params,Dim2=level_test[4]),
        request_base |> req_url_query(!!!params,Dim2=level_test[5]),
        request_base |> req_url_query(!!!params,Dim2=level_test[6]),
        request_base |> req_url_query(!!!params,Dim2=level_test[7]),
        request_base |> req_url_query(!!!params,Dim2=level_test[8])
      ) |> req_perform_parallel(on_error = "continue")
      
      test <- reqs |> 
        resps_data(
          function(resp){
            data <- read_json_raw(resp$body)
          }
        )
      if (!is.null(groups_chosen) & !is.null(groups_other)) {
        groups_chosen <- c(groups_chosen, groups_other)
      }
      # attempt to extract everything at once
      if(observation_requested==1 & isFALSE(individual)){
        req <- request_base |> req_url_query(op=2, varcd=indicators_current,lang="PT") |> req_perform()
        all <- req[["body"]] |> 
          read_json_raw()
      }else if(isFALSE(individual)){
        req <- request_base |> req_url_query(op=2, varcd=indicators_current,Dim1="T",lang="PT") |> req_perform()
        all <- req[["body"]] |> 
          read_json_raw()
      } else{
        all <- list(Sucesso = data.frame("Falso" = c("Falso")))
      }
          if("Falso" %in% all$Sucesso[[1]]){
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
                groups_chosen[k] == "ULS" ~ 2,
                # If all others fail, the default is the parish level
                TRUE ~ 1
              )
              # Sets the chosen values from the starting level
              codes_chosen <- codes_reference[[l]]
              geo_chosen <- geo_reference[[l]]
              level_names_chosen <- level_names_reference[l]
              # If it fails, then it increments until it finds a level with data
              while ("Falso" %in% names(test[[l]][[1]]) & l < 10) {
                l <- l + 1
                codes_chosen <- codes_reference[[l]]
                geo_chosen <- geo_reference[[l]]
                level_names_chosen <- level_names_reference[l]
                # Error condition when none of the selected levels have data
                if (l == 9) {
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
                # Call the INE API to gather the datasets for each code
                reqs <- map(codes_chosen,\(x) request_base|> req_url_query(!!!params,Dim2=x))
                reqs_raw <- reqs |> req_perform_parallel(on_error = "continue")
                # Function to safely read the JSON response, using purrr::safely
                safe_resps_data <- safely(function(resp) {
                  data <- read_json_raw(resp$body)
                  return(data)  # Return the data if successful
                })
                
                # Apply the safe function to all responses, continuing even if an error occurs
                results_raw <- reqs_raw |> 
                  resps_data(function(resp) {
                    safe_resps_data(resp)  # Apply the safe version of read_json_raw
                  })
                
                results <- map(results_raw$Dados, \(x) unpack_df(x)) |> list_rbind()
                observation_available_names <- names(results_raw$Dados[[1]])
                observation_available <- length(observation_available_names)
                # Checks if the requested number of observations is available
                if (observation_requested > observation_available) {
                  observation_used <- observation_available
                } else {
                  observation_used <- observation_requested
                }
                # Get names of the observations of interest
                observation_used_names <- c(observation_available_names[(observation_available - observation_used + 1):(observation_available)])
                df_all <- results |> 
                  dplyr::filter(obs %in% observation_available_names)
                result_list[[indicators_current]] <- df_all
              }
          }
        }else{
        # Extracts the data from  the INE API response
              df_all <- data.frame()
              results <- unpack_df(all$Dados[[1]])
              observation_available_names <- names(all$Dados[[1]])
              observation_available <- length(observation_available_names)
              # Checks if the requested number of observations is available
              if (observation_requested > observation_available) {
                observation_used <- observation_available
              } else {
                observation_used <- observation_requested
              }
              # Get names of the observations of interest
              observation_used_names <- c(observation_available_names[(observation_available - observation_used + 1):(observation_available)])
              if (!"obs" %in% names(results)) {
                # If 'obs' doesn't exist, create the 'obs' column with the value "0000"
                results <- results |> mutate(obs = "0000")
              }
              df_all <- results |> 
                dplyr::filter(obs %in% observation_available_names)
              
              success <- c(10)
                for (y in 1:length(level_test)) {
                  if("Falso" %in% names(test$Sucesso[[y]])){
                   next 
                  }else{
                   success <- sort(c(success,y))
                  }
                }
            combined_vector <- c(dicofre_2013, municipio_2013,  municipio_2002, nuts_3_2013, nuts_3_2002,  nuts_2_2013,  nuts_1_2013, pais)
            
            df_all <- df_all |> filter(geocod %in% combined_vector)
            
            if (any(c("Freguesia", "ACES", "ULS", "ARS", "Distrito") %in% groups_chosen) & (min(success) < 4 & 1 %in% success)) {
              geo_chosen <- geo_reference[[1]]
              df_all <- apply_filters(df_all, c("Município", "NUTS III", "NUTS II", "NUTS I", "País"), codes_reference,groups_chosen)
            }
            if (any(c("Município", "ACES", "ULS", "ARS", "Distrito") %in% groups_chosen) & (2 %in% success | min(success) == 2)) {
              geo_chosen <- geo_reference[[2]]
              df_all <- apply_filters(df_all, c("NUTS III", "NUTS II", "NUTS I", "País"), codes_reference,groups_chosen)
            }
            if ("NUTS III" %in% groups_chosen & (4 %in% success | min(success) == 4)) {
              df_all <- apply_filters(df_all, c("NUTS II", "NUTS I", "País"), codes_reference,groups_chosen)
            }
            if ("NUTS II" %in% groups_chosen & (6 %in% success | min(success) == 6)) {
              df_all <- apply_filters(df_all, c("NUTS I", "País"), codes_reference,groups_chosen)
            }
            if ("NUTS I" %in% groups_chosen & (7 %in% success | min(success) == 7)) {
              df_all <- apply_filters(df_all, c("País"), codes_reference,groups_chosen)
            }
            result_list[[indicators_current]] <- df_all
      }
      if (any(c("Distrito", "ACES", "ARS", "ULS") %in% groups_chosen) & min(success) < 4) {
        level_names_success <- level_names_reference[min(success)]
        if ("Distrito" %in% groups_chosen) {
          result_list[[indicators_current]] <- join_synthetic(result_list[[indicators_current]], "Distrito",level_names_success,selected_areas)
        }
        if ("ACES" %in% groups_chosen) {
          result_list[[indicators_current]] <- join_synthetic(result_list[[indicators_current]], "ACES",level_names_success,selected_areas)
        }
        if ("ULS" %in% groups_chosen) {
          result_list[[indicators_current]] <- join_synthetic(result_list[[indicators_current]], "ULS",level_names_success,selected_areas)
        }
        if ("ARS" %in% groups_chosen) {
          result_list[[indicators_current]] <- join_synthetic(result_list[[indicators_current]], "ARS",level_names_success,selected_areas)
        }
      } 
      if(!"valor" %in% names(result_list[[indicators_current]])){
        result_list[[indicators_current]] <- data.frame(geocod=0, geodsg=0, valor=0,obs=0, year=0)
      }
    }
  return(result_list)
}
chosen_group_options <- NULL
# Define UI for application that draws a histogram
# thematic::thematic_shiny(font_google("Lato"))

ui <- fluidPage(
  # Include custom CSS
  tags$head(
    tags$style(HTML("
    /* Style for larger screens */
    .responsive-row {
      display: flex; /* Establish flex container */
      flex-wrap: wrap; /* Allow wrapping if necessary */
    }
    .responsive-row .col-sm-4 {
      width: 33.33%; /* Each column takes up one-third of the space */
      box-sizing: border-box; /* Include padding and border in the element's total width and height */
    }

    /* Style for smaller screens */
    @media (max-width: 800px) {
      .responsive-row .col-sm-4 {
        width: 100%; /* Each column takes full width on small screens */
        float: none; /* Float is not needed with flexbox */
      }
    }
  ")),
  ),
  navbarPage(
  theme = bs_theme(base_font = font_google("Lato"),
  font_scale = -0.8, `enable-gradients` = TRUE, `enable-shadows` = TRUE
  ,spacer = "0.3rem", bootswatch = "minty"),
  # theme = bs_theme(), 
  # Change theme at will must activate bs_themer() in server
  "Extrator INE v0.43",
  nav_panel(
    "Extração de dados",
    useShinyjs(),
    fluidRow(
      class = "responsive-row",
      column(
        width = 4,align = "left",height = 60,
        imageOutput("sns_img1", height = "60px")
      ),
      column(
        width = 4,align = "left",height = 60,
        imageOutput("dgs_img1", height = "60px")
      ),
      column(
        width = 4,align = "left",height = 60,
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
          max = 20,
          value = 1
        ) |> tooltip("Se não houver dados do número pedido, é extraído o máximo."),
        # p("Se não houver dados do número pedido, é extraído o máximo."),
        actionButton("go", "Submeter", class = "btn-primary"),
        actionButton("stop", "Reiniciar", class = "btn-primary"),
        # Search bar for indicator
        selectizeInput(
          "indicators_search",
          "Selecionar indicadores:",
          choices = NULL,
          multiple = TRUE
        ) |> tooltip("Indicadores atualizados em 2023-03-06"),
        # p("Indicadores atualizados em 2023-03-06"),
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
            "ULS",
            "ARS"
          )
        ) |> tooltip(
          "Se não houver dados do pedido, será extraído o mais próximo possível."
        ),
        # p(
        #   "Se não houver dados do pedido, será extraído o mais próximo possível."
        # ),
        checkboxInput("select_all_checkbox", "Selecionar todos", FALSE),
        # Dropdown for additional desagregação options
        uiOutput("chosen_items_search"),
        checkboxInput("individual_checkbox",
                      "Extração individual",
                      FALSE)|> tooltip("Aumenta tempo de extração mas útil em indicadores com múltiplas dimensões com extração completa que não inclua níveis mais pequenos."),
        # p("Aumenta tempo de extração mas útil em indicadores com múltiplas dimensões com extração completa que não inclua níveis mais pequenos."),
        checkboxInput(
          "other_groups_checkbox",
          "Agrupar resultados por outros níveis",
          FALSE
        ),
        uiOutput("other_groups_search"),
        checkboxInput("graficos_checkbox",
                      "Fazer Gráficos",
                      FALSE),
        checkboxInput("meta_checkbox",
                      "Pedir Metainformação",
                      FALSE),
        checkboxInput(
          "show_debug",
          "Painel debug",
          FALSE
        )
      ,width = 3),
      # Show a plot of the generated distribution
      mainPanel(
        uiOutput("debug_panel_checkbox"),
        h2("Dados Recolhidos pelo Extractor"),
        uiOutput("error"),
        withSpinner(uiOutput("results_table"),type = 5, color = "#78C2AD", hide.ui = T)
      ,width = 9)
    )
  ),
  nav_panel(
    "Sobre",
    fluidRow(
      class = "responsive-row",
      column(
        width = 4,align = "left",height = 60,
        imageOutput("sns_img2", height = "60px")
      ),
      column(
        width = 4,align = "left",height = 60,
        imageOutput("dgs_img2", height = "60px")
      ),
      column(
        width = 4,align = "left",height = 60,
        imageOutput("ine_img2", height = "60px")
      )
    ),
    sidebarLayout(
      sidebarPanel(
        h4(strong("Autoria")),
        h4(
          strong("João Dionísio, Rafael Vasconcelos")
        )
      ),
      mainPanel(
        h3("Próximas melhorias"),
        p("- Corrigir o cálculo dos indicadores para distrito, ACES, ULS ARS quando não são contagens;"),
        p("- Remoção das variáveis para cálculo de dimensões não administrativas do INE (Distrito, ACES e ARS) - Para já deixo como validação"),
        p("- Automatizar a procura dos indicadores disponíveis;"),
        p("- Melhoria na manipulação de variáveis e visualizações;"),
        p("- Indicadores base em datasets base para evitar extração INE constante."),
        br(),
        h2("Changelog"),
        h3("V0.43"),
        h4("2024-06-16"),
        p("- Otimização de código para maior velocidade"),
        h3("V0.42"),
        h4("2024-03-27"),
        p("- Otimização de pesquisa de indicadores contínua;"),
        p("- Ponderar remoção do botão de reiniciar - manter para já"),
        p("- Corecção do mapeamento por ULS"),
        p("- Resolução de bug em que a seleção de todos os locais não permitia extração"),
        h4("Bugs Conhecidos"),
        h4("Em 2024-06-16"),
        p("- Problema na escolha de várias agregações superiores e inferiores que não permite filtro só do que foi pedido;"),
		    p("- Falha na conexão ao INE não dá feedback ao utilizador"),
        p("- Extrações de todas as freguesias do país em múltiplos indicadores leva a quebra do sistema.")
      )
    )
  )
)
)
#
server <- function(input, output, session) {
  # bs_themer()
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
      maxOptions = 30
    ),
    server = TRUE
  )
  
  chosen_group_options <- reactiveValues(available_items = NULL)
  
  values <- reactiveValues(select_all = FALSE)
  
  selected_input <- reactiveVal()
  # Update selected_input when input$chosen_group_dropdown or values$select_all changes
  observeEvent(c(input$chosen_group_dropdown, values$select_all), {
    selected_input(c(input$chosen_group_dropdown, values$select_all))
  })
  # Retrieves the list of available items for the chosen geographic level
  # Update chosen_group_options() whenever input$chosen_group_dropdown changes
  observeEvent(selected_input(), {
    if (input$chosen_group_dropdown == "Freguesia") {
      chosen_group_options$available_items <- geo_lookup$freguesia_2013
    } else if (input$chosen_group_dropdown == "Município") {
      chosen_group_options$available_items <- geo_lookup$municipio_2013
    } else if (input$chosen_group_dropdown == "Distrito") {
      chosen_group_options$available_items <- geo_lookup$distrito_2013
    } else if (input$chosen_group_dropdown == "NUTS III") {
      chosen_group_options$available_items <- geo_lookup$nuts3_2013
    } else if (input$chosen_group_dropdown == "NUTS II") {
      chosen_group_options$available_items <- geo_lookup$nuts2_2013
    } else if (input$chosen_group_dropdown == "NUTS I") {
      chosen_group_options$available_items <- geo_lookup$nuts1_2013
    } else if (input$chosen_group_dropdown == "País") {
      chosen_group_options$available_items <- geo_lookup$pais
    } else if (input$chosen_group_dropdown == "ACES") {
      chosen_group_options$available_items <- geo_lookup$aces_2022
    }else if (input$chosen_group_dropdown == "ULS") {
      chosen_group_options$available_items <- geo_lookup$uls_2023
    } else if (input$chosen_group_dropdown == "ARS") {
      chosen_group_options$available_items <- geo_lookup$ars_2022
    }
  })
  
  output$chosen_items_search <- renderUI({
    if (!is.null(chosen_group_options$available_items)) {
      selectizeInput(
        "chosen_items",
        "Selecionar itens a incluir:",
        choices = NULL,
        multiple = TRUE
      )
    }
  })
  # Render the dynamic dropdown menu with the available items for the chosen geographic level
  observeEvent(input$select_all_checkbox,{
    if (input$select_all_checkbox) {
      values$select_all <- TRUE
    } else {
      values$select_all <- FALSE
    }
  })

  observeEvent(selected_input(),{
    if (values$select_all==TRUE) {
      updateSelectizeInput(
        session,
        "chosen_items",
        choices = chosen_group_options$available_items,
        options = list(
                placeholder = "Barra de pesquisa",
                create = FALSE,
                multiple = TRUE
              ),
        selected = chosen_group_options$available_items,
        server = TRUE
      )
    } else {
      updateSelectizeInput(
            session,
            "chosen_items",
            choices = chosen_group_options$available_items,
            options = list(
              placeholder = "Barra de pesquisa",
              create = FALSE,
              multiple = TRUE
            ),
            server = TRUE
          )
    }
  })
  # Update the dropdown menu whenever available_items changes
  # observe({
  #   updateSelectizeInput(
  #     session,
  #     "chosen_items",
  #     choices = chosen_group_options$available_items,
  #     options = list(
  #       placeholder = "Barra de pesquisa",
  #       create = FALSE
  #     ),
  #     server = TRUE
  #   )
  # })
  # 
  
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
          tabPanel("Tabela de Freguesias", DTOutput("df"))
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
          "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País",
          "ACES",
          "ULS"
        )
    } else if (input$chosen_group_dropdown == "Município") {
      other_groups <-
        c(
          "Freguesia",
          "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País",
          "ACES",
          "ULS",
          "ARS"
        )
    } else if (input$chosen_group_dropdown == "Distrito") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          "ARS"# ,
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
          "Distrito",
          "NUTS II",
          "NUTS I",
          "País"
        )
    } else if (input$chosen_group_dropdown == "NUTS II") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          "Distrito",
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
          #"Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País", # ,
          "ARS"
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
          "ACES",
          "ULS"
        )
    } else if (input$chosen_group_dropdown == "ULS") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          # "Distrito",
          "NUTS III",
          "NUTS II",
          "NUTS I",
          "País",
          "ACES",
          "ARS"
        )
    }
    return(other_groups)
  })
  #
  output$other_groups_search <- renderUI({
    if (!is.null(chosen_group_options$available_items) & input$other_groups_checkbox == TRUE) {
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
  if(isFALSE(input$select_all_checkbox)){
    if(is.null(input$other_groups_list)|(!("Distrito"%in%input$other_groups_list)&!("ACES"%in%input$other_groups_list)&!("ARS"%in%input$other_groups_list)&!("ULS"%in%input$other_groups_list))){
    
	filtered <-
      geo_lookup |> filter(chosen_group_options$available_items %in% input$chosen_items)

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
    }else if("ARS" %in% input$other_groups_list){
      filtered <-
        geo_lookup |> filter(chosen_group_options$available_items %in% input$chosen_items)
      extra <- unique(filtered$ars_2022)
      extra1 <- unique(filtered$ars_2022_cod)
      # if("Distrito"%in% input$other_groups_list){
      #   extra2 <- unique(filtered$distrito_2013_cod)}else{
      #     extra2 <- c()
      #   }
      # if("ACES" %in% input$other_groups_list){
      #   extra3 <- unique(filtered$aces_2022_cod)}else{
      #     extra3 <- c()
      #   }
      filtered <-
        geo_lookup |> filter(ars_2022 %in% extra)
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
          f_municipio_2002 = f_municipio_2002,
          extra1=extra1
          # extra2=extra2,
          # extra3=extra3
        )
      )
    }else if("Distrito"%in%input$other_groups_list){
      filtered <-
        geo_lookup |> filter(chosen_group_options$available_items %in% input$chosen_items)
      extra <- unique(filtered$distrito_2013)
      extra1 <- unique(filtered$distrito_2013_cod)
      # if("ARS"%in%input$other_groups_list){
      #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
      # if("ACES"%in%input$other_groups_list){
      #   extra3 <- unique(filtered$aces_2022_cod)}else{extra3 <- c()        }
      filtered <-
        geo_lookup |> filter(distrito_2013 %in% extra)
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
          f_municipio_2002 = f_municipio_2002,
          extra1=extra1
          # extra2=extra2,
          # extra3=extra3
        )
      )
  }else if("ACES"%in%input$other_groups_list){
    filtered <-
      geo_lookup |> filter(chosen_group_options$available_items %in% input$chosen_items)
    extra <- unique(filtered$aces_2022)
    extra1 <- unique(filtered$aces_2022_cod)
    # if("ARS"%in%input$other_groups_list){
    #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
    # if("Distrito"%in%input$other_groups_list){
    #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
    filtered <-geo_lookup |> filter(aces_2022 %in% extra)
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
        f_municipio_2002 = f_municipio_2002,
        extra1=extra1
        # extra2=extra2,
        # extra3=extra3
      )
    )
    
  }else if("ULS"%in%input$other_groups_list){
    filtered <-
      geo_lookup |> filter(chosen_group_options$available_items %in% input$chosen_items)
    extra <- unique(filtered$uls_2023)
    # extra1 <- unique(filtered$aces_2022_cod)
    # if("ARS"%in%input$other_groups_list){
    #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
    # if("Distrito"%in%input$other_groups_list){
    #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
    filtered <-
      geo_lookup |> filter(uls_2023 %in% extra)
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
        f_municipio_2002 = f_municipio_2002,
        extra1="NA"
        # extra2=extra2,
        # extra3=extra3
      )
    )
    
  }
  } else{
	filtered <-
      geo_lookup
    # extra1 <- unique(filtered$aces_2022_cod)
    # if("ARS"%in%input$other_groups_list){
    #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
    # if("Distrito"%in%input$other_groups_list){
    #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
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
        f_municipio_2002 = f_municipio_2002,
        extra1="NA"
        # extra2=extra2,
        # extra3=extra3
      )
    )
  }
  })
  # Create a reactive function to ge the chosen indicator codes
  filtered_indicators <- reactive({
    f_indicators <- indicators |>
      filter(designacao %in% input$indicators_search) |>
      pull(codigo_de_difusao)
    return(f_indicators)
  })
  # Output the filtered dataset to the "df" table
  output$df <- renderDT({
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

  result_list_processed <- eventReactive(input$go, {
        result_list <- result_list
    if (length(filtered_indicators()) != 0 & nrow(filtered_area()$filtered_table) != 0) {
      # Replace this with your actual data fetching function
      result_list_updated <- ine.get(indicators = filtered_indicators(),selected_areas = filtered_area()$filtered_table, observation_requested = input$observation_slider,result_list = result_list,
                                     geo_reference = geo_reference,
                                     groups_chosen = input$chosen_group_dropdown,
                                     groups_other = input$other_groups_list,
                                     individual = input$individual_checkbox
      )
      return(result_list_reactive(result_list_updated))
      dimmension_chosen(c(input$other_groups_list, input$chosen_group_dropdown))
    } else {
      # Enable inputs
      NULL
    } 
  })
  
meta_list_processed <- eventReactive(input$go, {
     meta_list <- meta_list
    if(input$meta_checkbox) {
      # Replace this with your actual metadata fetching function
        meta_list_updated <- ine.meta(
          indicators = filtered_indicators(),
          meta_list = meta_list
        )
        return(meta_list_reactive(meta_list_updated))
    } else {
      NULL
    }
  }, ignoreNULL = TRUE)
  
  output$results_table <- NULL
  #
observeEvent(input$stop,{
  output$results_table <- NULL
}, ignoreNULL = TRUE)

eventReactive(input$go,{
  output$error <- renderUI({
   # if (length(filtered_indicators()) == 0) {
   #    tagList(
   #      br(),
   #      h1(strong("Não foi pedido nenhum indicador")),
   #      br()
   # }
    # Disable inputs
    shinyjs::disable(selector = "input")
    shinyjs::disable(selector = "select")
    shinyjs::disable(selector = "button")
    result_list <- result_list
    meta_list <- meta_list
    # Extract data from INE using the inputs from the UI
    if (length(filtered_indicators()) != 0 &
      nrow(filtered_area()$filtered_table) != 0) {
      # Render the tabs based on the reactive value
      output$results_table <- renderUI({
        result_list_updated <- ine.get(indicators = filtered_indicators(),selected_areas = filtered_area()$filtered_table, observation_requested = input$observation_slider,result_list = result_list,
                                       geo_reference = geo_reference,
                                       groups_chosen = input$chosen_group_dropdown,
                                       groups_other = input$other_groups_list,
                                       individual = input$individual_checkbox,
									                     all=input$select_all_checkbox
        )
        result_list_reactive(result_list_updated)
        dimmension_chosen(c(input$other_groups_list, input$chosen_group_dropdown))

        output$error <- NULL
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
            DTOutput(paste0(item, "_table")),
            downloadButton(paste0(item, "_download"), paste0(item, ".csv")),
            if(input$graficos_checkbox == TRUE){
              plotOutput(paste0(item,"_plot"))
              plotlyOutput(paste0(item,"_plotly"))
              plotOutput(paste0(item,"_plot1"),height = "1200px", width ="auto")
            },
            if(input$meta_checkbox == TRUE){
              downloadButton(paste0(item,"meta", "_download"), paste0(item,"meta",".csv"))
            }
          )
        })
        # Return a tabsetPanel with the tabs
        do.call(tabsetPanel, tabs)
      })
    } else if (length(filtered_indicators()) == 0) {
      output$error <- renderUI({
        tagList(
          br(),
          h1(strong("Não foi pedido nenhum indicador")),
          br()
        )
      }
      )
    } else {
      tagList(
        br(),
        h1(strong(
          "Não foi pedida nenhuma desagregação"
        )),
        br()
      )
 }
  shinyjs::enable(selector = "input")
  shinyjs::enable(selector = "select")
  shinyjs::enable(selector = "button")
}) 
}) 
observeEvent(input$go,{
  output$results_table <- renderUI({
    req(result_list_processed())
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
        DTOutput(paste0(item, "_table")),
        downloadButton(paste0(item, "_download"), paste0(item, ".csv")),
        if(input$graficos_checkbox == TRUE){
          plotOutput(paste0(item,"_plot"))
          plotlyOutput(paste0(item,"_plotly"))
          plotOutput(paste0(item,"_plot1"),height = "1200px", width ="auto")
        },
        if(input$meta_checkbox == TRUE){
          req(meta_list_processed ())
          downloadButton(paste0(item,"meta", "_download"), paste0(item,"meta",".csv"))
        }
      )
    })
    # Return a tabsetPanel with the tabs
    do.call(tabsetPanel, tabs)
  })
})

# 
  # Render dataTables and download handlers and simple plot when result_list_reactive changes
observe({
    lapply(names(result_list_reactive()), function(item) {
      output[[paste0(item, "_table")]] <- renderDT({
        # Get the data from result_list_reactive
        data <- result_list_reactive()[[item]]
        data <- data %>%
          # dplyr::filter(geodsg %in% input$chosen_items | geocod %in% filtered_area()$extra1| geocod %in% filtered_area()$extra2| geocod %in% filtered_area()$extra3 )%>%
          mutate(valor = as.numeric(valor))%>%
          mutate(year = str_sub(obs,-4))%>%
          arrange(obs, year)
        # Return a dataTable with the data
        DT::datatable(data,filter = list(position = 'top', clear = FALSE))
        
      })
      output[[paste0(item, "_download")]] <- downloadHandler(
        filename = function() {
          paste0(item, ".csv")
        },
        content = function(file) {
          # Get the data from result_list_reactive
          data <- result_list_reactive()[[item]]
          # Write the data to a csv file , BOM (Byte Order Mark) at the beginning of the file; format 'UTF-8
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
          # Write the data to a csv file BOM (Byte Order Mark) at the beginning of the file; format 'UTF-8
          fwrite(data, file, sep = ";", bom = TRUE)
        }
      )}
      if(input$graficos_checkbox == TRUE){ 
      output[[paste0(item, "_plot")]] <- renderPlot({
        # This line retrieves the full name of the item from the 'indicators' dataframe based on its code
        full_name <- indicators$designacao[indicators$codigo_de_difusao == item]
        # This line retrieves the data for the current item from the reactive function and converts it to a dataframe
        data1 <- as.data.frame(result_list_reactive()[[item]]) %>%
          # This line converts the 'valor' column to numeric and creates a 'year' column based on the last 4 characters of 'obs'
          mutate(valor = as.numeric(valor),
                 year = str_sub(obs, -4)) %>%
          # This line sorts the data by 'obs' and 'year'
          arrange(obs, year)
        
        if(any(str_detect(colnames(data1), "dim"))){
          data1  <- data1%>%
            select(ends_with("t")|!starts_with("dim"))
        }
        # This line creates a ggplot object with the data1 dataframe as input
        if(any(str_detect(colnames(data1), "dim"))){
          if(sum(str_detect(colnames(data1), "dim"))== 1){
            data1<- data1%>% 
              mutate(aggregate = interaction(geodsg, dim_3_t, sep = ", "))%>%
              summarise(valor= sum(valor, na.rm = TRUE), .by = c(obs,aggregate,geodsg,dim_3_t,year))%>%
              arrange(obs, year)
            
            
            p <- ggplot2::ggplot() +
              # This line adds a line layer with 'obs' on the x-axis, 'valor' on the y-axis, 'geodsg' as color, and 'geodsg' as the grouping variable
              geom_line(data = data1,
                        aes(x = factor(obs, levels = unique(obs), ordered = TRUE),
                            y = valor,
                            colour = dim_3_t,
                            group =  interaction(geodsg, dim_3_t, sep = "-")),
                        linewidth = 1.2) +
              facet_wrap(~geodsg, scales = "free")+
              # This line rotates the x-axis labels by 90 degrees
              scale_x_discrete(guide = guide_axis(angle = 90)) +
              # This line adds a legend for the color variable, using the name 'Localização Geográfica'
              scale_color_hue(name = "Localização Geográfica") +
              scale_color_hue(direction=1, aesthetics = "group")+
              # This line sets the chart limits to remove extra white space
              coord_cartesian(expand = FALSE) +
              # This line adds a chart title, subtitle, and caption
              labs(x = "Observações",
                   y = "Valor",
                   title = full_name,
                   subtitle = paste0("Últimas ", length(unique(data1$obs)), " Observações"),
                   caption = "Fonte dos Dados: INE") +
              # This line sets the chart style to minimal and customizes the font sizes
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 12, face = "bold"),
                    axis.title.y = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12),
                    legend.title = element_text(size = 12))
            
            }else if(sum(str_detect(colnames(data1), "dim"))== 2){
              #WORKING ON IT
              p <- ggplot2::ggplot() +
                # This line adds a line layer with 'obs' on the x-axis, 'valor' on the y-axis, 'geodsg' as color, and 'geodsg' as the grouping variable
                geom_line(data = data1,
                          aes(x = factor(obs, levels = unique(obs), ordered = TRUE),
                              y = valor,
                              colour = as.factor(geocod),
                              group = interaction(geodsg, dim_3_t, dim_4_t, sep = "-")),
                          linewidth = 1.2) +
                # This line rotates the x-axis labels by 90 degrees
                scale_x_discrete(guide = guide_axis(angle = 90)) +
                # This line adds a legend for the color variable, using the name 'Localização Geográfica'
                scale_color_discrete(name = "Localização Geográfica") +
                # This line sets the chart limits to remove extra white space
                coord_cartesian(expand = FALSE) +
                # This line adds a chart title, subtitle, and caption
                labs(x = "Observações",
                     y = "Valor",
                     title = full_name,
                     subtitle = paste0("Últimas ", length(unique(data1$obs)), " Observações"),
                     caption = "Fonte dos Dados: INE") +
                # This line sets the chart style to minimal and customizes the font sizes
                theme_minimal() +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      plot.subtitle = element_text(size = 12, face = "bold"),
                      axis.title.y = element_text(size = 12),
                      axis.title.x = element_text(size = 12),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      legend.title = element_text(size = 12))
              
            }else{
              p <- NULL
            }
          }else{
              p <- ggplot2::ggplot() +
                # This line adds a line layer with 'obs' on the x-axis, 'valor' on the y-axis, 'geodsg' as color, and 'geodsg' as the grouping variable
                geom_line(data = data1,
                          aes(x = factor(obs, levels = unique(obs), ordered = TRUE),
                              y = valor,
                              colour = as.factor(as.character(geocod)),
                              group = geodsg),
                          linewidth = 1.2) +
                # This line rotates the x-axis labels by 90 degrees
                scale_x_discrete(guide = guide_axis(angle = 90)) +
                # This line adds a legend for the color variable, using the name 'Localização Geográfica'
                scale_color_discrete(name = "Localização Geográfica") +
                # This line sets the chart limits to remove extra white space
                coord_cartesian(expand = FALSE) +
                # This line adds a chart title, subtitle, and caption
                labs(x = "Observações",
                     y = "Valor",
                     title = full_name,
                     subtitle = paste0("Últimas ", length(unique(data1$obs)), " Observações"),
                     caption = "Fonte dos Dados: INE") +
                # This line sets the chart style to minimal and customizes the font sizes
                theme_minimal() +
                theme(plot.title = element_text(size = 14, face = "bold"),
                      plot.subtitle = element_text(size = 12, face = "bold"),
                      axis.title.y = element_text(size = 12),
                      axis.title.x = element_text(size = 12),
                      axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12),
                      legend.title = element_text(size = 12))
            }
            
            # This line prints the ggplot object
            print(p)
            return(p)
      })
      output[[paste0(item, "_plotly")]] <- renderPlotly({
        p1 <- ggplotly()
        # This line prints the ggplot object
        print(p1)
      })
      output[[paste0(item, "_plot1")]] <- renderPlot({
        # This line retrieves the full name of the item from the 'indicators' dataframe based on its code
        full_name <- indicators$designacao[indicators$codigo_de_difusao == item]
        # This line retrieves the data for the current item from the reactive function and converts it to a dataframe
        data1 <- as.data.frame(result_list_reactive()[[item]]) %>%
          # dplyr::filter(geodsg %in% input$chosen_items | geocod %in% filtered_area()$extra1| geocod %in% filtered_area()$extra2|geocod %in% filtered_area()$extra3 )%>%
          # This line converts the 'valor' column to numeric and creates a 'year' column based on the last 4 characters of 'obs'
          mutate(valor = as.numeric(valor),
                 year = str_sub(obs, -4)) %>%
          # This line sorts the data by 'obs' and 'year'
          arrange(obs, year)
        if(any(str_detect(colnames(data1), "dim"))){
          data1  <- data1%>%
            select(ends_with("t")|!starts_with("dim"))
        }
        # This line creates a ggplot object with the data1 dataframe as input
        p <- ggplot2::ggplot() +
          # This line adds a line layer with 'obs' on the x-axis, 'valor' on the y-axis, 'geodsg' as color, and 'geodsg' as the grouping variable
          geom_line(data = data1,
                    aes(x = factor(obs, levels = unique(obs), ordered = TRUE),
                        y = valor,
                        colour = as.factor(geocod),
                        group = geodsg),
                    linewidth = 1.2)
        
        if(any(str_detect(colnames(data1), "dim"))){
          if(sum(str_detect(colnames(data1), "dim"))== 1){
          p <- p+
            facet_wrap(~dim_3_t, scales = "free")}
          else if(sum(str_detect(colnames(data1), "dim"))== 2){
            p <- p+
              facet_wrap(~dim_3_t+dim_4_t, scales = "free")}
          else{
            p <- NULL
          }
        }
          # This line rotates the x-axis labels by 90 degrees
        p <- p +
          scale_x_discrete(guide = guide_axis(angle = 90)) +
          # This line adds a legend for the color variable, using the name 'Localização Geográfica'
          scale_color_discrete(name = "Localização Geográfica") +
          # This line sets the chart limits to remove extra white space
          coord_cartesian(expand = FALSE) +
          # This line adds a chart title, subtitle, and caption
          labs(x = "Observações",
               y = "Valor",
               title = full_name,
               subtitle = paste0("Últimas ", length(unique(data1$obs)), " Observações"),
               caption = "Fonte dos Dados: INE") +
          # This line sets the chart style to minimal and customizes the font sizes
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12),
                axis.title.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 12),
                legend.title = element_text(size = 12))
        
        # This line prints the ggplot object
        print(p)
      })}
    })
  # Enable inputs
  shinyjs::enable(selector = "input")
  shinyjs::enable(selector = "select")
  shinyjs::enable(selector = "button")
  })
}


            
# Run the application
shinyApp(ui, server)

# h4("V0.41"),
# h4("2023-12-08"),
# p("- Otimização de indicadores pequenos para extração completa;"),
# p("- Dada opção para extração por ULS"),
# p("- Dada opção para extração individual"),
# p("- Dada opção para criação de gráficos"),
# h4("V0.3.6"),
# h4("2023-05-17"),
# p("- Corrigir o cálculo dos indicadores para distrito, ACES, ARS quando há múltiplas dimensões (até 5 dimensões);"),
# p("- Adicionado filtro em tabela para manipulação dos resultados;"),
# p("- Otimização do código dos gráficos"),
# p("- Criação de botão para selecionar tudo."),
# p("- Comentado código."),
# br(),
# h3("V0.3.1"),
# h4("2023-03-21"),
# p("- Melhoria da adaptação das visualizações;"),
# br(),
# h4("V0.3"),
# h4("2023-03-18"),
# p("- Melhorias visuais"),
# p("- Feedback ao utilizador de funcionamento da função principal"),
# p("- Visualização dos dados recolhidos"),
# br(),
# h4("V0.2.2"),
# h4("2023-03-16"),
# p("- Possibilitada a transferência de metadados."),
# p("- Alteração do ficheiro de saída com UTF-8 para manter caracteres especiais."),
# p("- Remoção temporária de opções de outros níveis que causavam erro."),
# p("- Redução no número de chamadas ao servidor, se houver redundâncias."),
# br(),
# h4("V0.2.0"),
# h4("2023-03-15"),
# p("- Reescrita das instruções;"),
# p("- Estruturação dos resultados com várias níveis geográficos em simultâneo."),
# br(),
# h4("V0.1.1"),
# h4("2023-03-14"),
# p("- Redução do tempo entre chamadas ao servidor;"),
# p("- Otimização do número de uniões dos resultados recebidos;"),
# p("- Correção de erro na listagem dos municípios."),