# -----------------------------------------------------------------------------
# |         Aplicación Shiny para Mapa de Centros Educativos                |
# |                  (Versión 4.2 - Lectura desde Excel)                      |
# -----------------------------------------------------------------------------

# --- 1. Carga de Librerías ---
# install.packages(c("shiny", "leaflet", "dplyr", "readxl", "htmltools", "DT", "tidyr"))

library(shiny)
library(leaflet)
library(dplyr)
library(readxl)      # <--- LIBRERÍA NUEVA para leer Excel
library(htmltools)
library(DT)
library(tidyr)
library(rsconnect)

# --- 2. Lectura de Datos desde Archivo Excel ---
# IMPORTANTE: El archivo "mapa_mmm_muestra.xlsx" debe estar en la misma
# carpeta que este script de la aplicación (app.R).

tryCatch({
  mapa_data_raw <- readxl::read_excel("mapa_mmm_muestra.xlsx")
}, error = function(e) {
  stop("Error al leer el archivo Excel 'mapa_mmm_muestra.xlsx'. ",
       "Asegúrate de que el archivo exista en el directorio de la app. Error original: ", e$message)
})


# --- 3. Limpieza y Preparación de los Datos ---
mapa <- mapa_data_raw %>%
  mutate(
    latitud = suppressWarnings(as.numeric(latitud)),
    longitud = suppressWarnings(as.numeric(longitud)),
    departamento = tidyr::replace_na(departamento, "No especificado"),
    distrito = tidyr::replace_na(distrito, "No especificado"),
    cluster = tidyr::replace_na(as.character(cluster), "No especificado")
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud)) %>%
  filter(sede_codigo != 86027)


# --- 4. Definición de la Interfaz de Usuario (UI) ---
ui <- fluidPage(
  
  titlePanel("Mapa Interactivo de Centros Educativos"),
  
  fluidRow(
    column(width = 2,
           h4("Filtros del Mapa"),
           hr(),
           selectInput(
             inputId = "filtro_muestra",
             label = "¿En muestra?",
             choices = c("Todos" = "todos", "Sí" = "1", "No" = "0"),
             selected = "todos"
           ),
           selectInput(
             inputId = "filtro_depto",
             label = "Departamento:",
             choices = c("Todos" = "todos", sort(unique(mapa$departamento)))
           ),
           selectInput(inputId = "filtro_distrito", label = "Distrito:", choices = NULL),
           selectInput(inputId = "filtro_cluster", label = "Clúster:", choices = NULL),
           hr(),
           
           h2("Eurolatina",align = "center",style = "color:blue"),
           style = "text-align: center;",
           a(img(src = "https://media.licdn.com/dms/image/v2/C560BAQE93z4FbxjLCg/company-logo_200_200/company-logo_200_200/0/1630617827310/eurolatina_logo?e=1755129600&v=beta&t=EDbY6wBnBUVsxh1Ch_A252TjB29yW9GpEjpUCn-d-rE", class = "custom-logo",style="margin-top: 0px; padding-center:36px;padding-bottom:5px", height = 50)),
           
           helpText("Los filtros de Distrito y Clúster se actualizan según la selección.")
    ),
    column(width = 10,
           leafletOutput("mapa_centros", height = "55vh")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(width = 12,
           h4("Datos de los Centros Educativos Filtrados"),
           DT::dataTableOutput("tabla_datos")
    )
  )
)


# --- 5. Definición de la Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Filtros Dinámicos (en cascada) ---
  observeEvent(input$filtro_depto, {
    if (input$filtro_depto == "todos") {
      opciones_distrito <- c("Todos" = "todos", sort(unique(mapa$distrito)))
    } else {
      opciones_distrito <- mapa %>%
        filter(departamento == input$filtro_depto) %>%
        pull(distrito) %>%
        unique() %>%
        sort()
      opciones_distrito <- c("Todos" = "todos", opciones_distrito)
    }
    updateSelectInput(session, "filtro_distrito", choices = opciones_distrito)
  })
  
  observe({
    data_filtrada_cluster <- mapa
    if (input$filtro_depto != "todos") {
      data_filtrada_cluster <- data_filtrada_cluster %>% filter(departamento == input$filtro_depto)
    }
    if (!is.null(input$filtro_distrito) && input$filtro_distrito != "todos") {
      data_filtrada_cluster <- data_filtrada_cluster %>% filter(distrito == input$filtro_distrito)
    }
    opciones_cluster <- c("Todos" = "todos", sort(unique(data_filtrada_cluster$cluster)))
    updateSelectInput(session, "filtro_cluster", choices = opciones_cluster)
  })
  
  # --- Datos Filtrados (para mapa y tabla) ---
  datos_filtrados <- reactive({
    data_a_filtrar <- mapa
    if (input$filtro_muestra != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(EntraMuestreo == as.numeric(input$filtro_muestra))
    }
    if (input$filtro_depto != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(departamento == input$filtro_depto)
    }
    if (!is.null(input$filtro_distrito) && input$filtro_distrito != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(distrito == input$filtro_distrito)
    }
    if (!is.null(input$filtro_cluster) && input$filtro_cluster != "todos") {
      data_a_filtrar <- data_a_filtrar %>% filter(cluster == input$filtro_cluster)
    }
    return(data_a_filtrar)
  })
  
  # --- Salidas (Mapa y Tabla) ---
  output$mapa_centros <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "Mapa") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      setView(lng = -88.89653, lat = 13.794185, zoom = 9)
  })
  
  observe({
    df <- datos_filtrados()
    
    popup_content <- paste0(
      "<b>Código:</b> ", htmlEscape(df$sede_codigo), "<br>",
      "<b>Centro Educativo:</b> ", htmlEscape(df$nombre_del_centro_educativo), "<br>",
      "<b>Departamento:</b> ", htmlEscape(df$departamento), "<br>",
      "<b>Distrito:</b> ", htmlEscape(df$distrito), "<br>",
      "<b>Clúster:</b> ", htmlEscape(df$cluster), "<br>",
      "<b>Grupo T:</b> ", htmlEscape(df$GruposT), "<br>",
      "<b>Grupo N:</b> ", htmlEscape(df$GrupoN), "<br>",
      "<b>¿Entra a Muestra?:</b> ", htmlEscape(df$EntraMuestreo), "<br>",
      "<b>Cuota de Muestreo:</b> ", htmlEscape(df$Cuota_Muestreo_Docentes)
    )
    
    leafletProxy("mapa_centros", data = df) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(
        lng = ~longitud, lat = ~latitud,
        popup = ~popup_content,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("Mapa", "Satélite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$tabla_datos <- DT::renderDataTable({
    df_tabla <- datos_filtrados() %>%
      select(
        `Código` = sede_codigo,
        `Nombre del Centro` = nombre_del_centro_educativo,
        Departamento = departamento,
        Distrito = distrito,
        Clúster = cluster,
        `En Muestra` = EntraMuestreo,
        `Cuota Maestros` = CuotaMuestraDocentes,
        `Matrícula Total` = total_matricula
      )
    DT::datatable(
      df_tabla,
      rownames = FALSE,
      class = 'cell-border stripe',
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      )
    )
  })
}

# --- 6. Ejecución de la Aplicación ---
shinyApp(ui = ui, server = server)