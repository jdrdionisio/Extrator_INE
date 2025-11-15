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
geo_lookup <- fread("datasets/geo_linkage_2024_v6.csv")
# Remove `unknown` and `abroad` from the reference table
geo_lookup <- geo_lookup |>
  filter(!dicofre_2013 %in% c("0", "999999"))

geo_reference <- list(
  freguesia_2025 = geo_lookup,
  freguesia_2013 = geo_lookup[c(3, 5:37)],
  municipio_2024 = geo_lookup[5:37],
  municipio_2013 = geo_lookup[5:37],
  municipio_2002 = geo_lookup[5:37],
  nuts3_2024 = geo_lookup[c(17:37)],
  nuts3_2013 = geo_lookup[c(17:37)],
  nuts3_2002 = geo_lookup[c(17:37)],
  nuts2_2024 = geo_lookup[c(22:37)],
  nuts2_2013 = geo_lookup[c(22:37)],
  nuts1_2024 = geo_lookup[c(26:37)],
  nuts1_2013 = geo_lookup[c(26:37)],
  pais = geo_lookup[c(28:37)],
  aces_2022 = geo_lookup[c(5:10, 17:37)],
  uls_2024 = geo_lookup[c(5:10, 17:37)],
  regioes_2024 = geo_lookup[c(5:10, 17:37)],
  ars_2022 = geo_lookup[c(5:10, 17:37)]
)
# Retrieve the available indicators
tryCatch(
  {
    temp_file <- tempfile(fileext = ".xlsx")
    req <- httr2::request("https://smi.ine.pt/Indicador/Exportacao?tipo=0") |>
      httr2::req_perform(path = temp_file)

    indicators <- read_excel(temp_file, skip = 14) |>
      clean_names() |>
      filter(disponivel_no_portal == "Sim") |>
      distinct(designacao, .keep_all = TRUE)
    date <- Sys.Date()
  },
  error = function(e) {
    indicators <- import(here("datasets", "Indicators.xlsx"))
    date <- "2025-05-12"
  }
)

# Prepare an empty list for the results
result_list <- list()
meta_list <- list()
chosen_group_options <- NULL

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
    "Município" = c(
      codes_reference$municipio_2024,
      codes_reference$municipio_2013,
      codes_reference$municipio_2002
    ),
    "NUTS III" = c(
      codes_reference$nuts3_2024,
      codes_reference$nuts3_2013,
      codes_reference$nuts3_2002
    ),
    "NUTS II" = c(codes_reference$nuts2_2024, codes_reference$nuts2_2013),
    "NUTS I" = codes_reference$nuts1_2013,
    "País" = codes_reference$pais
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

#Adicionada região
synthetic_level <- data.frame(
  Distrito = c("distrito_2013", "distrito_2013_cod"),
  ACES = c("aces_2022", "aces_2022_cod"),
  ARS = c("ars_2022", "ars_2022_cod"),
  REGIAO = c("regiao_2024"),
  ULS = c("uls_2024")
)

join_synthetic <- function(
  df,
  synthetic_group,
  level_names_success,
  geo_chosen
) {
  num_dims <- sum(str_detect(colnames(df), "dim"))
  col_dims <- generate_dim_columns(num_dims)
  # cat(level_names_success)
  # cat(names(geo_chosen))
  geo_chosen <- geo_chosen |>
    rename("geocod" = {{ level_names_success }})
  # cat("\n")
  # cat(names(geo_chosen))
  syn <- synthetic_level[[synthetic_group]]
  # cat(syn)
  summarise_by <- c("obs", syn[1], col_dims)
  # cat(summarise_by)
  select <- c("geocod", "geodsg", col_dims, "valor", "obs")
  new_df <- df |>
    # Adds the geographical information
    left_join(geo_chosen, multiple = "first") |>
    summarise(
      geocod = .data[[syn[2]]],
      geodsg = .data[[syn[1]]],
      valor = sum(valor, na.rm = TRUE),
      # obs = obs,
      .by = all_of(summarise_by)
    ) |>
    select(all_of(select)) |>
    mutate(
      geocod = as.character(geocod)
    ) |>
    filter(!is.na(geocod)) |>
    unique()
  # print(new_df)
  df <- bind_rows(df, new_df)
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
ine.get <- function(
  indicators,
  selected_areas,
  observation_requested,
  result_list,
  geo_reference,
  groups_chosen,
  groups_other,
  individual,
  all,
  progress_callback = NULL
) {
  codes_reference <- list(
    dicofre_2025 = unique(selected_areas$dicofre_2025),
    dicofre_2013 = unique(selected_areas$dicofre_2013),
    municipio_2024 = unique(selected_areas$municipio_2024_cod),
    municipio_2013 = unique(selected_areas$municipio_2013_cod),
    municipio_2002 = unique(selected_areas$municipio_2002_cod),
    nuts_3_2024 = unique(selected_areas$nuts3_2024_cod),
    nuts_3_2013 = unique(selected_areas$nuts3_2013_cod),
    nuts_3_2002 = unique(selected_areas$nuts3_2002_cod),
    nuts_2_2024 = unique(selected_areas$nuts2_2024_cod),
    nuts_2_2013 = unique(selected_areas$nuts2_2013_cod),
    nuts_1_2013 = unique(selected_areas$nuts1_2013_cod),
    pais = unique(selected_areas$pais_cod),
    ""
  )
  names(codes_reference) <- c(
    "dicofre_2025",
    "dicofre_2013",
    "municipio_2024",
    "municipio_2013",
    "municipio_2002",
    "nuts3_2024",
    "nuts3_2013",
    "nuts3_2002",
    "nuts2_2024",
    "nuts2_2013",
    "nuts1_2013",
    "pais",
    ""
  )

  level_names_reference <- c(
    "dicofre_2025",
    "dicofre_2013",
    "municipio_2024_cod",
    "municipio_2013_cod",
    "municipio_2002_cod",
    "nuts3_2024_cod",
    "nuts3_2013_cod",
    "nuts3_2002_cod",
    "nuts2_2024_cod",
    "nuts2_2013_cod",
    "nuts1_2013_cod",
    "pais_cod",
    ""
  )
  level_test <- c(
    "0302FK", # Tests 2025 parishes
    "011102", # Tests parishes
    "1941823", # Tests 2024 municipalities
    "16E0111", # Tests 2013 municipalities
    "1610111", # Tests 2002 municipalities
    "194", # Tests 2024 NUTS III
    "16E", # Tests 2013 NUTS III
    "161", # Tests 2002 NUTS III
    "19", # Tests 2024 NUTS II
    "16", # Tests 2013 NUTS II
    "1", # Tests NUTS II
    "PT" # Tests country
  )
  for (i in 1:length(indicators)) {
    indicators_current <- indicators[i]

    if (is.function(progress_callback)) {
      progress_callback(paste0(
        "A testar o indicador ",
        i,
        "/",
        length(indicators),
        ": ",
        indicators_current
      ))
    }

    request_base <- request("https://www.ine.pt/ine/json_indicador/pindica.jsp")
    params <- list(op = 2, varcd = indicators_current, Dim1 = "T", lang = "PT")

    reqs <- list(
      request_base |> req_url_query(!!!params, Dim2 = level_test[1]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[2]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[3]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[4]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[5]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[6]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[7]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[8]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[9]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[10]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[11]),
      request_base |> req_url_query(!!!params, Dim2 = level_test[12])
    ) |>
      req_perform_parallel(on_error = "continue")

    # test <- lapply(responses, function(resp) {
    #   if (
    #     inherits(resp, "httr2_failure") ||
    #       is.null(resp) ||
    #       is.null(resp$body) ||
    #       length(resp$body) == 0
    #   ) {
    #     return(list(Sucesso = list("Falso" = "Falso")))
    #   }
    #   tryCatch(
    #     {
    #       read_json_raw(resp$body)
    #     },
    #     error = function(e) {
    #       list(Sucesso = list("Falso" = "Falso"))
    #     }
    #   )
    # })
    test <- reqs |>
      resps_data(
        function(resp) {
          data <- read_json_raw(resp$body)
        }
      )

    if (!is.null(groups_chosen) & !is.null(groups_other)) {
      groups_chosen <- c(groups_chosen, groups_other)
    }

    if (observation_requested == 1 & isFALSE(individual)) {
      cat("Pedir tudo")
      req <- request_base |>
        req_url_query(op = 2, varcd = indicators_current, lang = "PT") |>
        req_perform()
      all_data <- req[["body"]] |> read_json_raw()
    } else if (isFALSE(individual)) {
      cat("Pedir tudo 2")
      req <- request_base |>
        req_url_query(
          op = 2,
          varcd = indicators_current,
          Dim1 = "T",
          lang = "PT"
        ) |>
        req_perform()
      all_data <- req[["body"]] |> read_json_raw()
    } else {
      all_data <- list(Sucesso = data.frame("Falso" = c("Falso")))
    }
    if ("Falso" %in% all_data$Sucesso[[1]]) {
      cat("Tentar tirar poucos")
      success <- c(11)
      for (k in 1:length(groups_chosen)) {
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
          groups_chosen[k] == "REGIAO" ~ 2,
          groups_chosen[k] == "ULS" ~ 2,
          TRUE ~ 1
        )
        codes_chosen <- codes_reference[[l]]
        geo_chosen <- geo_reference[[l]]
        level_names_chosen <- level_names_reference[l]
        while ("Falso" %in% names(test[[l]][[1]]) & l < 11) {
          l <- l + 1
          codes_chosen <- codes_reference[[l]]
          geo_chosen <- geo_reference[[l]]
          level_names_chosen <- level_names_reference[l]
          if (l == 10) {
            errorCondition(
              "Condições selecionadas sem resultados para este indicador."
            )
          }
        }
        if (l %in% success) {
          next
        } else {
          df_all <- data.frame()
          reqs <- map(codes_chosen, \(x) {
            request_base |> req_url_query(!!!params, Dim2 = x)
          })
          reqs_raw <- reqs |> req_perform_parallel(on_error = "continue")
          safe_resps_data <- safely(function(resp) {
            data <- read_json_raw(resp$body)
            if (!("Dados" %in% names(data))) {
              data$Dados <- data.frame(
                obs = "0000",
                valor = "0"
              )
            }
            return(data)
          })

          results_raw <- reqs_raw |>
            resps_data(
              function(resp) {
                safe_resps_data(resp)
              }
            )

          results <- map(results_raw$Dados, \(x) unpack_df(x)) |> list_rbind()
          observation_available_names <- names(results_raw$Dados[[1]])
          observation_available <- length(observation_available_names)
          if (observation_requested > observation_available) {
            observation_used <- observation_available
          } else {
            observation_used <- observation_requested
          }
          observation_used_names <- c(observation_available_names[
            (observation_available -
              observation_used +
              1):(observation_available)
          ])
          if (!"obs" %in% names(results)) {
            results <- results |> mutate(obs = "0000")
          }
          df_all <- results |>
            dplyr::filter(obs %in% observation_available_names)

          result_list[[indicators_current]] <- df_all
        }
      }
    } else {
      df_all <- data.frame()
      results <- unpack_df(all_data$Dados[[1]])
      observation_available_names <- names(all_data$Dados[[1]])
      observation_available <- length(observation_available_names)
      if (observation_requested > observation_available) {
        observation_used <- observation_available
      } else {
        observation_used <- observation_requested
      }
      observation_used_names <- c(observation_available_names[
        (observation_available - observation_used + 1):(observation_available)
      ])
      if (!"obs" %in% names(results)) {
        results <- results |> mutate(obs = "0000")
      }

      df_all <- results |>
        dplyr::filter(obs %in% observation_available_names)

      success <- c(11)
      for (y in 1:length(level_test)) {
        if ("Falso" %in% names(test$Sucesso[[y]])) {
          next
        } else {
          success <- sort(c(success, y))
        }
      }
      cat("success: ")
      cat(success)
      cat("\n")
      combined_vector <- c(
        codes_reference$dicofre_2025,
        codes_reference$dicofre_2013,
        codes_reference$municipio_2024,
        codes_reference$municipio_2013,
        codes_reference$municipio_2002,
        codes_reference$nuts_3_2024,
        codes_reference$nuts_3_2013,
        codes_reference$nuts_3_2002,
        codes_reference$nuts_2_2024,
        codes_reference$nuts_2_2013,
        codes_reference$nuts_1_2013,
        codes_reference$pais
      )

      if ("geocod" %in% names(df_all)) {
        df_all <- df_all |> filter(geocod %in% combined_vector)
      }
      if (
        any(
          c("Freguesia", "ACES", "ULS", "ARS", "Distrito") %in% groups_chosen
        ) &
          (min(success) <= 5 & 1 %in% success)
      ) {
        cat("1")
        geo_chosen <- geo_reference[[1]]
        df_all <- apply_filters(
          df_all,
          c("Município", "NUTS III", "NUTS II", "NUTS I", "País"),
          codes_reference,
          groups_chosen
        )
      }
      if (
        any(
          c("Município", "ACES", "ULS", "ARS", "Distrito") %in% groups_chosen
        ) &
          (3 %in% success | min(success) == 3)
      ) {
        cat("2")
        geo_chosen <- geo_reference[[3]]
        df_all <- apply_filters(
          df_all,
          c("NUTS III", "NUTS II", "NUTS I", "País"),
          codes_reference,
          groups_chosen
        )
      }
      if (
        "NUTS III" %in% groups_chosen & (6 %in% success | min(success) == 6)
      ) {
        cat("3")
        df_all <- apply_filters(
          df_all,
          c("NUTS II", "NUTS I", "País"),
          codes_reference,
          groups_chosen
        )
      }
      if ("NUTS II" %in% groups_chosen & (9 %in% success | min(success) == 9)) {
        cat("4")
        df_all <- apply_filters(
          df_all,
          c("NUTS I", "País"),
          codes_reference,
          groups_chosen
        )
      }
      if (
        "NUTS I" %in% groups_chosen & (11 %in% success | min(success) == 11)
      ) {
        cat("5")
        df_all <- apply_filters(
          df_all,
          c("País"),
          codes_reference,
          groups_chosen
        )
      }
      result_list[[indicators_current]] <- df_all
    }
    if (
      any(c("Distrito", "ACES", "ARS", "ULS") %in% groups_chosen) &
        min(success) <= 5
    ) {
      level_names_success <- level_names_reference[min(success)]
      cat("level_names_success: ")
      cat(level_names_success)
      if ("Distrito" %in% groups_chosen) {
        result_list[[indicators_current]] <- join_synthetic(
          result_list[[indicators_current]],
          "Distrito",
          level_names_success,
          selected_areas
        )
      }
      if ("ACES" %in% groups_chosen) {
        result_list[[indicators_current]] <- join_synthetic(
          result_list[[indicators_current]],
          "ACES",
          level_names_success,
          selected_areas
        )
      }
      if ("ULS" %in% groups_chosen) {
        result_list[[indicators_current]] <- join_synthetic(
          result_list[[indicators_current]],
          "ULS",
          level_names_success,
          selected_areas
        )
      }
      if ("ARS" %in% groups_chosen) {
        result_list[[indicators_current]] <- join_synthetic(
          result_list[[indicators_current]],
          "ARS",
          level_names_success,
          selected_areas
        )
      }
    }
    if (!"valor" %in% names(result_list[[indicators_current]])) {
      result_list[[indicators_current]] <- data.frame(
        geocod = 0,
        geodsg = 0,
        valor = 0,
        obs = 0,
        year = 0
      )
    }
  }
  return(result_list)
}
chosen_group_options <- NULL

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
    .responsive-row {
      display: flex;
      flex-wrap: wrap;
    }
    .responsive-row .col-sm-4 {
      width: 33.33%;
      box-sizing: border-box;
    }
    @media (max-width: 800px) {
      .responsive-row .col-sm-4 {
        width: 100%;
        float: none;
      }
    }
  "
    )),
  ),
  navbarPage(
    theme = bs_theme(
      base_font = font_google("Lato"),
      font_scale = -0.8,
      `enable-gradients` = TRUE,
      `enable-shadows` = TRUE,
      spacer = "0.3rem",
      bootswatch = "minty"
    ),
    "Extrator de Indicadores do INE",
    nav_panel(
      "Extração de dados",
      useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            "indicators_search",
            "Selecionar indicadores:",
            choices = NULL,
            multiple = TRUE
          ) |>
            tooltip(paste0("Indicadores atualizados em ", date)),
          selectizeInput(
            "indicators_code_search",
            "Procurar por código:",
            choices = NULL,
            multiple = TRUE
          ),
          textAreaInput(
            "pasted_codes",
            "Colar códigos (separados por vírgula, ponto e vírgula, ou nova linha):",
            rows = 3
          ),
          sliderInput(
            "observation_slider",
            "Número de observações a pedir:",
            min = 1,
            max = 20,
            value = 1
          ) |>
            tooltip(
              "Se não houver dados do número pedido, é extraído o máximo."
            ),
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
              "Região",
              "ULS",
              "ARS"
            )
          ) |>
            tooltip(
              "Se não houver dados do pedido, será extraído o mais próximo possível."
            ),
          checkboxInput("select_all_checkbox", "Selecionar todos", FALSE),
          uiOutput("chosen_items_search"),
          p("Se Extração Normal falhar, tentar extração individual"),
          checkboxInput("individual_checkbox", "Extração individual", FALSE) |>
            tooltip(
              "Aumenta tempo de extração mas útil em indicadores com múltiplas dimensões com extração completa que não inclua níveis mais pequenos."
            ),
          checkboxInput(
            "other_groups_checkbox",
            "Agrupar resultados por outros níveis",
            FALSE
          ),
          uiOutput("other_groups_search"),
          checkboxInput("graficos_checkbox", "Fazer Gráficos", FALSE),
          checkboxInput("meta_checkbox", "Pedir Metainformação", FALSE),
          checkboxInput(
            "show_debug",
            "Painel debug",
            FALSE
          ),
          actionButton("go", "Submeter", class = "btn-primary"),
          actionButton("stop", "Reiniciar", class = "btn-primary")
        ),
        mainPanel(
          width = 9,
          layout_columns(
            col_widths = c(4, 4, 4),
            imageOutput("sns_img1", height = "60px"),
            imageOutput("dgs_img1", height = "60px"),
            imageOutput("ine_img1", height = "60px")
          ),
          uiOutput("debug_panel_checkbox"),
          h2("Dados Recolhidos pelo Extractor"),
          uiOutput("error"),
          withSpinner(
            uiOutput("results_table"),
            type = 5,
            color = "#78C2AD",
            hide.ui = T
          )
        )
      )
    ),
    nav_panel(
      "Sobre",
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
          p("- Melhoria na manipulação de variáveis e visualizações;"),
          p("- Indicadores base em datasets base para evitar extração INE constante."),
          br(),
          h2("Changelog"),
          h3("V0.5"),
          h4("2025-11-12"),
          p("- Otimização de código para as novas divisões"),
          p("- Automatizar a procura dos indicadores disponíveis;"),
          p("- Criação de esquema para copiar e colar indicadores do INE;"),
          h3("V0.43"),
          h4("2024-06-16"),
          p("- Otimização de código para maior velocidade"),
          h3("V0.42"),
          h4("2024-03-27"),
          p("- Otimização de pesquisa de indicadores contínua;"),
          p("- Ponderar remoção do botão de reiniciar - manter para já"),
          p("- Corecção do mapeamento por ULS"),
          p(
            "- Resolução de bug em que a seleção de todos os locais não permitia extração"
          ),
          h4("Bugs Conhecidos"),
          h4("Em 2024-06-16"),
          p(
            "- Problema na escolha de várias agregações superiores e inferiores que não permite filtro só do que foi pedido;"
          ),
          p("- Falha na conexão ao INE não dá feedback ao utilizador"),
          p(
            "- Extrações de todas as freguesias do país em múltiplos indicadores leva a quebra do sistema."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$sns_img1 <- renderImage(
    {
      list(src = "www/SNS.png", height = 60)
    },
    deleteFile = F
  )
  output$dgs_img1 <- renderImage(
    {
      list(src = "www/DGS.png", height = 60)
    },
    deleteFile = F
  )
  output$ine_img1 <- renderImage(
    {
      list(src = "www/INE.gif", height = 60)
    },
    deleteFile = F
  )

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

  updateSelectizeInput(
    session,
    "indicators_code_search",
    choices = indicators$codigo_de_difusao,
    options = list(
      placeholder = "Barra de Pesquisa por código",
      create = FALSE,
      maxOptions = 30
    ),
    server = TRUE
  )

  chosen_group_options <- reactiveValues(available_items = NULL)
  values <- reactiveValues(select_all = FALSE)
  selected_input <- reactiveVal()

  observeEvent(c(input$chosen_group_dropdown, values$select_all), {
    selected_input(c(input$chosen_group_dropdown, values$select_all))
  })

  observeEvent(selected_input(), {
    if (input$chosen_group_dropdown == "Freguesia 2025") {
      chosen_group_options$available_items <- geo_lookup$freguesia_2025
    } else if (input$chosen_group_dropdown == "Município") {
      chosen_group_options$available_items <- geo_lookup$municipio_2013
    } else if (input$chosen_group_dropdown == "Distrito") {
      chosen_group_options$available_items <- geo_lookup$distrito_2013
    } else if (input$chosen_group_dropdown == "NUTS III") {
      chosen_group_options$available_items <- unique(c(
        geo_lookup$nuts3_2024,
        geo_lookup$nuts3_2013
      ))
    } else if (input$chosen_group_dropdown == "NUTS II") {
      chosen_group_options$available_items <- geo_lookup$nuts2_2013
    } else if (input$chosen_group_dropdown == "NUTS I") {
      chosen_group_options$available_items <- geo_lookup$nuts1_2013
    } else if (input$chosen_group_dropdown == "País") {
      chosen_group_options$available_items <- geo_lookup$pais
    } else if (input$chosen_group_dropdown == "ACES") {
      chosen_group_options$available_items <- geo_lookup$aces_2022
    } else if (input$chosen_group_dropdown == "ULS") {
      chosen_group_options$available_items <- geo_lookup$uls_2024
    } else if (input$chosen_group_dropdown == "Região") {
      chosen_group_options$available_items <- geo_lookup$regiao_2024
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

  observeEvent(input$select_all_checkbox, {
    values$select_all <- input$select_all_checkbox
  })

  observeEvent(selected_input(), {
    if (values$select_all == TRUE) {
      updateSelectizeInput(
        session,
        "chosen_items",
        choices = chosen_group_options$available_items,
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
          multiple = TRUE,
          maxOptions = 4000
        ),
        server = TRUE
      )
    }
  })

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
          "Região",
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
          "Região",
          "ARS"
        )
    } else if (input$chosen_group_dropdown == "Distrito") {
      other_groups <-
        c(
          "Freguesia",
          "Município",
          "Região",
          "ARS" # ,
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
          "Região",
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
          "Região",
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
          "Região",
          "ARS"
        )
    }
    return(other_groups)
  })

  output$other_groups_search <- renderUI({
    if (input$other_groups_checkbox == TRUE) {
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
  filtered_area <- reactive({
    if (isFALSE(input$select_all_checkbox)) {
      if (
        is.null(input$other_groups_list) |
          (!("Distrito" %in% input$other_groups_list) &
            !("ACES" %in% input$other_groups_list) &
            !("Região" %in% input$other_groups_list) &
            !("ARS" %in% input$other_groups_list) &
            !("ULS" %in% input$other_groups_list))
      ) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)

        # Sets lists of codes for debug panel
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002
          )
        )
      } else if ("Distrito" %in% input$other_groups_list) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)
        extra <- unique(filtered$distrito_2013)
        filtered <-
          geo_lookup |> filter(distrito_2013 %in% extra)
        # Sets lists of codes for debug panel
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002
          )
        )
      } else if ("ARS" %in% input$other_groups_list) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)
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
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002,
            extra1 = extra1
            # extra2=extra2,
            # extra3=extra3
          )
        )
      } else if ("Distrito" %in% input$other_groups_list) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)
        extra <- unique(filtered$distrito_2013)
        extra1 <- unique(filtered$distrito_2013_cod)
        # if("ARS"%in%input$other_groups_list){
        #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
        # if("ACES"%in%input$other_groups_list){
        #   extra3 <- unique(filtered$aces_2022_cod)}else{extra3 <- c()        }
        filtered <-
          geo_lookup |> filter(distrito_2013 %in% extra)
        # Sets lists of codes for debug panel
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002,
            extra1 = extra1
            # extra2=extra2,
            # extra3=extra3
          )
        )
      } else if ("ACES" %in% input$other_groups_list) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)
        extra <- unique(filtered$aces_2022)
        extra1 <- unique(filtered$aces_2022_cod)
        # if("ARS"%in%input$other_groups_list){
        #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
        # if("Distrito"%in%input$other_groups_list){
        #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
        filtered <- geo_lookup |> filter(aces_2022 %in% extra)
        # Sets lists of codes for debug panel
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002,
            extra1 = extra1
            # extra2=extra2,
            # extra3=extra3
          )
        )
      } else if ("ULS" %in% input$other_groups_list) {
        filtered <-
          geo_lookup |>
          filter(chosen_group_options$available_items %in% input$chosen_items)
        extra <- unique(filtered$uls_2023)
        # extra1 <- unique(filtered$aces_2022_cod)
        # if("ARS"%in%input$other_groups_list){
        #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
        # if("Distrito"%in%input$other_groups_list){
        #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
        filtered <-
          geo_lookup |> filter(uls_2023 %in% extra)
        # Sets lists of codes for debug panel
        f_freguesia_2025 <- filtered |>
          pull(dicofre_2025) |>
          unique()
        f_freguesia_2013 <- filtered |>
          pull(dicofre_2013) |>
          unique()
        f_municipio_2024 <- filtered |>
          pull(municipio_2024_cod) |>
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
            f_freguesia_2025 = f_freguesia_2025,
            f_freguesia_2013 = f_freguesia_2013,
            f_municipio_2024 = f_municipio_2024,
            f_municipio_2013 = f_municipio_2013,
            f_municipio_2002 = f_municipio_2002,
            extra1 = NA,
            # extra2=extra2,
            # extra3=extra3
          )
        )
      }
    } else {
      filtered <-
        geo_lookup
      # extra1 <- unique(filtered$aces_2022_cod)
      # if("ARS"%in%input$other_groups_list){
      #   extra2 <- unique(filtered$ars_2022_cod)}else{extra2 <- c()        }
      # if("Distrito"%in%input$other_groups_list){
      #   extra3 <- unique(filtered$distrito_2013_cod)}else{extra3 <- c()        }
      # Sets lists of codes for debug panel
      f_freguesia_2025 <- filtered |>
        pull(dicofre_2025) |>
        unique()
      f_freguesia_2013 <- filtered |>
        pull(dicofre_2013) |>
        unique()
      f_municipio_2024 <- filtered |>
        pull(municipio_2024_cod) |>
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
          f_freguesia_2025 = f_freguesia_2025,
          f_freguesia_2013 = f_freguesia_2013,
          f_municipio_2024 = f_municipio_2024,
          f_municipio_2013 = f_municipio_2013,
          f_municipio_2002 = f_municipio_2002,
          extra1 = NA,
          # extra2=extra2,
          # extra3=extra3
        )
      )
    }
  })
  filtered_indicators <- reactive({
    # Get indicators from the main search bar
    selected_designacao <- input$indicators_search

    # Get indicators from the code search bar
    selected_codes <- input$indicators_code_search

    # Process pasted codes
    pasted_codes_raw <- input$pasted_codes
    # Split by any combination of commas, semicolons, spaces, tabs, or newlines
    pasted_codes_vec <- str_split(pasted_codes_raw, "[\\s,;\\t\\n]+")[[1]]
    pasted_codes_vec <- pasted_codes_vec[pasted_codes_vec != ""]

    # Validate and pad codes
    valid_pasted_codes <- c()
    invalid_pasted_codes <- c()

    for (code in pasted_codes_vec) {
      if (nchar(code) < 7) {
        code <- str_pad(code, 7, "left", "0")
      }
      if (code %in% indicators$codigo_de_difusao) {
        valid_pasted_codes <- c(valid_pasted_codes, code)
      } else {
        invalid_pasted_codes <- c(invalid_pasted_codes, code)
      }
    }

    # Show a notification for invalid codes
    if (length(invalid_pasted_codes) > 0) {
      showNotification(
        paste(
          "Códigos inválidos ou não encontrados:",
          paste(invalid_pasted_codes, collapse = ", ")
        ),
        type = "warning",
        duration = 10
      )
    }

    # Combine all selected indicators
    codes_from_designacao <- indicators |>
      filter(designacao %in% selected_designacao) |>
      pull(codigo_de_difusao)

    final_codes <- unique(c(
      codes_from_designacao,
      selected_codes,
      valid_pasted_codes
    ))

    return(final_codes)
  })

  output$df <- renderDT({
    filtered_area()$filtered_table
  })
  output$filtered_indicators <- renderPrint({
    filtered_indicators()
  })
  output$filtered_freguesia <- renderPrint({
    filtered_area()$f_freguesia_2025
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

  result_list_reactive <- reactiveVal()
  meta_list_reactive <- reactiveVal()
  dimmension_chosen <- reactiveVal()

  observeEvent(
    input$stop,
    {
      result_list_reactive(NULL)
      meta_list_reactive(NULL)
      output$results_table <- NULL
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$go, {
    shinyjs::disable(selector = "input, button, select")

    if (length(filtered_indicators()) == 0) {
      output$error <- renderUI({
        h1(strong("Não foi pedido nenhum indicador"))
      })
      shinyjs::enable(selector = "input, button, select")
      return()
    }
    if (isFALSE(input$select_all_checkbox) && length(input$chosen_items) == 0) {
      output$error <- renderUI({
        h1(strong("Não foi pedida nenhuma desagregação"))
      })
      shinyjs::enable(selector = "input, button, select")
      return()
    }

    output$error <- NULL

    withProgress(message = 'A processar o seu pedido...', value = 0, {
      n_indicators <- length(filtered_indicators())
      progress_per_indicator <- 1 /
        (n_indicators + as.integer(input$meta_checkbox))

      # Use a safe version of ine.get
      safe_ine_get <- safely(ine.get)

      results <- safe_ine_get(
        indicators = filtered_indicators(),
        selected_areas = filtered_area()$filtered_table,
        observation_requested = input$observation_slider,
        result_list = result_list,
        geo_reference = geo_reference,
        groups_chosen = input$chosen_group_dropdown,
        groups_other = input$other_groups_list,
        individual = input$individual_checkbox,
        all = input$select_all_checkbox,
        progress_callback = function(detail_message) {
          incProgress(amount = 0, detail = detail_message)
        }
      )

      if (!is.null(results$error)) {
        showNotification(
          paste("Erro ao extrair dados:", results$error$message),
          type = "error",
          duration = 15
        )
        shinyjs::enable(selector = "input, button, select")
        return()
      }

      result_list_reactive(results$result)
      dimmension_chosen(c(input$other_groups_list, input$chosen_group_dropdown))
      incProgress(
        amount = n_indicators * progress_per_indicator,
        detail = "Dados extraídos."
      )

      if (input$meta_checkbox) {
        setProgress(detail = "A extrair os metadados...")
        metadata <- ine.meta(
          indicators = filtered_indicators(),
          meta_list = meta_list
        )
        meta_list_reactive(metadata)
        incProgress(
          amount = progress_per_indicator,
          detail = "Metadados extraídos."
        )
      }

      setProgress(1, detail = "Concluído!")
    })

    shinyjs::enable(selector = "input, button, select")
  })

  output$results_table <- renderUI({
    req(result_list_reactive())
    items <- names(result_list_reactive())

    tabs <- lapply(items, function(item) {
      full_name_vec <- indicators$designacao[
        indicators$codigo_de_difusao == item
      ]
      full_name <- if (length(full_name_vec) > 0) full_name_vec[1] else item
      title <- substr(full_name, 1, 20)
      # Source - https://stackoverflow.com/q/10294284
      # Posted by Qbik, modified by community. See post 'Timeline' for change history
      # Retrieved 2025-11-15, License - CC BY-SA 4.0
      x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="
      name_clean <- gsub("[[:punct:]]", "", title) # no libraries needed

      nav_panel(
        title,
        h4(strong(full_name)),
        fluidRow(column(12, DTOutput(paste0(item, "_table")))),
        hr(),
        fluidRow(
          tagList(
            column(
              2,
              downloadButton(paste0(item, "_download"), paste0(item, ".csv"))
            ),
            if (isTRUE(input$meta_checkbox)) {
              req(meta_list_reactive())
              column(
                2,
                downloadButton(
                  paste0(item, "meta", "_download"),
                  paste0(item, "meta", ".csv")
                )
              )
            }
          )
        ),
        if (isTRUE(input$graficos_checkbox)) {
          tagList(
            hr(),
            plotOutput(paste0(item, "_plot")),
            plotlyOutput(paste0(item, "_plotly")),
            plotOutput(
              paste0(item, "_plot1"),
              height = "1200px",
              width = "auto"
            )
          )
        }
      )
    })
    do.call(navset_card_tab, tabs)
  })

  observe({
    req(result_list_reactive())
    lapply(names(result_list_reactive()), function(item) {
      full_name_vec <- indicators$designacao[
        indicators$codigo_de_difusao == item
      ]
      full_name <- if (length(full_name_vec) > 0) full_name_vec[1] else item
      title <- substr(full_name, 1, 20)
      # Source - https://stackoverflow.com/q/10294284
      # Posted by Qbik, modified by community. See post 'Timeline' for change history
      # Retrieved 2025-11-15, License - CC BY-SA 4.0
      name_clean <- gsub("[[:punct:]]", "", full_name)

      output[[paste0(item, "_table")]] <- renderDT({
        data <- result_list_reactive()[[item]]
        data <- data %>%
          mutate(valor = as.numeric(valor), year = str_sub(obs, -4)) %>%
          arrange(obs, year)
        DT::datatable(data, filter = list(position = 'top', clear = FALSE))
      })

      output[[paste0(item, "_download")]] <- downloadHandler(
        filename = function() {
          paste0(item, format(Sys.Date(), "%Y%m%d"), name_clean, ".csv")
        },
        content = function(file) {
          data <- result_list_reactive()[[item]]
          fwrite(data, file, sep = ";", bom = TRUE)
        }
      )

      if (isTRUE(input$meta_checkbox)) {
        req(meta_list_reactive())
        output[[paste0(item, "meta", "_download")]] <- downloadHandler(
          filename = function() {
            paste0(item, "meta", ".csv")
          },
          content = function(file) {
            data <- meta_list_reactive()[[item]]
            fwrite(data, file, sep = ";", bom = TRUE)
          }
        )
      }

      if (isTRUE(input$graficos_checkbox)) {
        output[[paste0(item, "_plot")]] <- renderPlot({
          full_name <- indicators$designacao[
            indicators$codigo_de_difusao == item
          ]
          data1 <- as.data.frame(result_list_reactive()[[item]]) %>%
            mutate(valor = as.numeric(valor), year = str_sub(obs, -4)) %>%
            arrange(obs, year)

          if (any(str_detect(colnames(data1), "dim"))) {
            data1 <- data1 %>%
              select(ends_with("t") | !starts_with("dim"))
          }

          p <- if (any(str_detect(colnames(data1), "dim"))) {
            if (sum(str_detect(colnames(data1), "dim")) == 1) {
              data1 <- data1 %>%
                mutate(aggregate = interaction(geodsg, dim_3_t, sep = ", ")) %>%
                summarise(
                  valor = sum(valor, na.rm = TRUE),
                  .by = c(obs, aggregate, geodsg, dim_3_t, year)
                ) %>%
                arrange(obs, year)

              ggplot2::ggplot() +
                geom_line(
                  data = data1,
                  aes(
                    x = factor(obs, levels = unique(obs), ordered = TRUE),
                    y = valor,
                    colour = dim_3_t,
                    group = interaction(geodsg, dim_3_t, sep = "-")
                  ),
                  linewidth = 1.2
                ) +
                facet_wrap(~geodsg, scales = "free")
            } else {
              NULL
            }
          } else {
            ggplot2::ggplot() +
              geom_line(
                data = data1,
                aes(
                  x = factor(obs, levels = unique(obs), ordered = TRUE),
                  y = valor,
                  colour = as.factor(as.character(geocod)),
                  group = geodsg
                ),
                linewidth = 1.2
              )
          }

          if (!is.null(p)) {
            p <- p +
              scale_x_discrete(guide = guide_axis(angle = 90)) +
              scale_color_discrete(name = "Localização Geográfica") +
              coord_cartesian(expand = FALSE) +
              labs(
                x = "Observações",
                y = "Valor",
                title = full_name,
                subtitle = paste0(
                  "Últimas ",
                  length(unique(data1$obs)),
                  " Observações"
                ),
                caption = "Fonte dos Dados: INE"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12),
                legend.title = element_text(size = 12)
              )
            print(p)
          }
        })

        output[[paste0(item, "_plotly")]] <- renderPlotly({
          # This logic should be self-contained or call a reactive that is
        })

        output[[paste0(item, "_plot1")]] <- renderPlot({
          full_name <- indicators$designacao[
            indicators$codigo_de_difusao == item
          ]
          data1 <- as.data.frame(result_list_reactive()[[item]]) %>%
            mutate(valor = as.numeric(valor), year = str_sub(obs, -4)) %>%
            arrange(obs, year)

          if (any(str_detect(colnames(data1), "dim"))) {
            data1 <- data1 %>%
              select(ends_with("t") | !starts_with("dim"))
          }

          p <- ggplot2::ggplot() +
            geom_line(
              data = data1,
              aes(
                x = factor(obs, levels = unique(obs), ordered = TRUE),
                y = valor,
                colour = as.factor(geocod),
                group = geodsg
              ),
              linewidth = 1.2
            )

          if (any(str_detect(colnames(data1), "dim"))) {
            if (sum(str_detect(colnames(data1), "dim")) == 1) {
              p <- p + facet_wrap(~dim_3_t, scales = "free")
            } else if (sum(str_detect(colnames(data1), "dim")) == 2) {
              p <- p + facet_wrap(~ dim_3_t + dim_4_t, scales = "free")
            }
          }

          p <- p +
            scale_x_discrete(guide = guide_axis(angle = 90)) +
            scale_color_discrete(name = "Localização Geográfica") +
            coord_cartesian(expand = FALSE) +
            labs(
              x = "Observações",
              y = "Valor",
              title = full_name,
              subtitle = paste0(
                "Últimas ",
                length(unique(data1$obs)),
                " Observações"
              ),
              caption = "Fonte dos Dados: INE"
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              legend.title = element_text(size = 12)
            )
          print(p)
        })
      }
    })
  })
}
# Run the application
shinyApp(ui, server)
