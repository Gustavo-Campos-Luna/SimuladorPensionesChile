# C# Simulador Interactivo de Pensiones en Chile
# Desarrollado como proyecto de portafolio

# Carga de librerías necesarias
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(highcharter)
library(readr)
library(shinythemes)

# Función para calcular la pensión estimada
calcular_pension <- function(edad_actual, edad_jubilacion, esperanza_vida, 
                             ingreso_mensual, aumento_anual, 
                             cotizacion, comision_afp, aporte_empleador,
                             rentabilidad_anual, anos_lagunas, periodo_lagunas) {
  
  # Años hasta la jubilación (incluido el año de jubilación)
  anos_cotizacion <- edad_jubilacion - edad_actual + 1
  
  # Inicializar dataframe para tracking anual
  df_simulacion <- data.frame(
    ano = 1:anos_cotizacion,
    edad = edad_actual:(edad_actual + anos_cotizacion - 1),
    ingreso_mensual = numeric(anos_cotizacion),
    cotizacion_anual = numeric(anos_cotizacion),
    comision_anual = numeric(anos_cotizacion),
    rentabilidad = numeric(anos_cotizacion),
    saldo_acumulado = numeric(anos_cotizacion)
  )
  
  # Calcular los ingresos para cada año con el aumento anual
  for (i in 1:anos_cotizacion) {
    if (i == 1) {
      df_simulacion$ingreso_mensual[i] <- ingreso_mensual
    } else {
      df_simulacion$ingreso_mensual[i] <- df_simulacion$ingreso_mensual[i-1] * (1 + aumento_anual/100)
    }
  }
  
  # Determinar años con lagunas previsionales
  tiene_laguna <- rep(FALSE, anos_cotizacion)
  if (anos_lagunas > 0) {
    if (periodo_lagunas == "Aleatorio") {
      # Asignar lagunas aleatoriamente
      anos_con_lagunas <- sample(1:anos_cotizacion, anos_lagunas)
      tiene_laguna[anos_con_lagunas] <- TRUE
    } else if (periodo_lagunas == "Inicio de carrera") {
      # Lagunas al inicio
      tiene_laguna[1:min(anos_lagunas, anos_cotizacion)] <- TRUE
    } else if (periodo_lagunas == "Mitad de carrera") {
      # Lagunas en la mitad
      inicio_lagunas <- max(1, round(anos_cotizacion/2) - round(anos_lagunas/2))
      fin_lagunas <- min(anos_cotizacion, inicio_lagunas + anos_lagunas - 1)
      tiene_laguna[inicio_lagunas:fin_lagunas] <- TRUE
    }
  }
  
  # Calcular el saldo acumulado año a año
  saldo <- 0
  for (i in 1:anos_cotizacion) {
    if (!tiene_laguna[i]) {
      cotizacion_mensual <- df_simulacion$ingreso_mensual[i] * (cotizacion/100)
      comision_mensual <- df_simulacion$ingreso_mensual[i] * (comision_afp/100)
      aporte_empleador_mensual <- df_simulacion$ingreso_mensual[i] * (aporte_empleador/100)
      
      # Calcular montos anuales
      df_simulacion$cotizacion_anual[i] <- (cotizacion_mensual + aporte_empleador_mensual) * 12
      df_simulacion$comision_anual[i] <- comision_mensual * 12
    } else {
      # En años con lagunas no hay cotización ni comisión
      df_simulacion$cotizacion_anual[i] <- 0
      df_simulacion$comision_anual[i] <- 0
    }
    
    # Actualizar saldo considerando rentabilidad
    saldo <- saldo * (1 + rentabilidad_anual/100) + df_simulacion$cotizacion_anual[i] - df_simulacion$comision_anual[i]
    df_simulacion$rentabilidad[i] <- if(i>1) saldo - df_simulacion$saldo_acumulado[i-1] - df_simulacion$cotizacion_anual[i] + df_simulacion$comision_anual[i] else 0
    df_simulacion$saldo_acumulado[i] <- saldo
  }
  
  # Usar la esperanza de vida proporcionada por el usuario
  anos_pension <- esperanza_vida - edad_jubilacion
  
  # Cálculo simple de pensión mensual (Retiro Programado simplificado)
  pension_mensual_rp <- saldo / (anos_pension * 12)
  
  # Cálculo simple para Renta Vitalicia (con factor dinámico)
  factor_renta_vitalicia <- 0.92  # Valor predeterminado, será reemplazado por el input del usuario
  pension_mensual_rv <- pension_mensual_rp * factor_renta_vitalicia
  
  # Tasa de reemplazo (con respecto al último sueldo)
  tasa_reemplazo_rp <- pension_mensual_rp / df_simulacion$ingreso_mensual[anos_cotizacion] * 100
  tasa_reemplazo_rv <- pension_mensual_rv / df_simulacion$ingreso_mensual[anos_cotizacion] * 100
  
  # Retornar resultados
  return(list(
    saldo_final = saldo,
    pension_mensual_rp = pension_mensual_rp,
    pension_mensual_rv = pension_mensual_rv,
    tasa_reemplazo_rp = tasa_reemplazo_rp,
    tasa_reemplazo_rv = tasa_reemplazo_rv,
    simulacion = df_simulacion,
    tiene_laguna = tiene_laguna,
    ultimo_sueldo = df_simulacion$ingreso_mensual[anos_cotizacion]
  ))
}

# Definir la interfaz de usuario
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Simulador Interactivo de Pensiones en Chile"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Sección Demografía
      h4("Datos Demográficos", class = "section-header"),
      numericInput("edad_actual", "Edad actual:", 30, min = 18, max = 60),
      radioButtons("genero", "Género:", choices = c("Hombre", "Mujer")),
      numericInput("edad_jubilacion", "Edad de jubilación:", 65, min = 60, max = 70),
      numericInput("esperanza_vida", "Esperanza de vida (años):", 85, min = 65, max = 110),
      
      # Sección Ingresos
      h4("Ingresos", class = "section-header"),
      numericInput("ingreso_mensual", "Ingreso mensual imponible (CLP):", 800000, min = 350000),
      sliderInput("aumento_anual", "% Aumento anual del sueldo:", 2, min = 0, max = 10, step = 0.5),
      
      # Sección Cotización
      h4("Cotización", class = "section-header"),
      numericInput("cotizacion", "% Cotización obligatoria:", 10, min = 10, max = 20),
      selectInput("comision_afp", "Comisión mensual AFP (%):",
                  choices = c("0.57%" = 0.57, "0.77%" = 0.77, "1.19%" = 1.19, "1.44%" = 1.44),
                  selected = 0.77),
      numericInput("aporte_empleador", "Aporte del empleador (%):", 0, min = 0, max = 10),
      sliderInput("rentabilidad_anual", "Rentabilidad esperada anual (%):", 5, min = 2, max = 8, step = 0.5),
      
      # Sección Lagunas
      h4("Lagunas Previsionales", class = "section-header"),
      sliderInput("anos_lagunas", "Años sin cotización:", 0, min = 0, max = 20, step = 1),
      selectInput("periodo_lagunas", "Ubicación de las lagunas:",
                  choices = c("Aleatorio", "Inicio de carrera", "Mitad de carrera", "Cerca de jubilación")),
      
      # Sección Modalidad
      h4("Modalidad de Pensión", class = "section-header"),
      radioButtons("modalidad_pension", "Tipo de pensión:", 
                   choices = c("Retiro Programado", "Renta Vitalicia")),
      conditionalPanel(
        condition = "input.modalidad_pension == 'Renta Vitalicia'",
        sliderInput("factor_renta_vitalicia", "Factor Renta Vitalicia (% del RP):", 
                    min = 80, max = 100, value = 92, step = 1)
      ),
      
      # Sección avanzada (placeholder)
      h4("Opciones Avanzadas", class = "section-header"),
      fileInput("archivo_sueldo", "Cargar historial de sueldos (CSV):"),
      actionButton("simular", "Simular Pensión", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Resultados Principales",
                 fluidRow(
                   column(4,
                          div(class = "card shadow-sm p-3 mb-4 bg-white rounded",
                              h5("Saldo Acumulado al Jubilar", style = "font-weight:bold;"),
                              span(textOutput("saldo_acumulado_text"), style = "font-size: 24px; color: #2c3e50; font-weight: bold;")
                          )
                   ),
                   column(4,
                          div(class = "card shadow-sm p-3 mb-4 bg-white rounded",
                              h5("Pensión Mensual Estimada", style = "font-weight:bold;"),
                              span(textOutput("pension_mensual_text"), style = "font-size: 24px; color: #2c3e50; font-weight: bold;")
                          )
                   ),
                   column(4,
                          div(class = "card shadow-sm p-3 mb-4 bg-white rounded",
                              h5("Tasa de Reemplazo", style = "font-weight:bold;"),
                              span(textOutput("tasa_reemplazo_text"), style = "font-size: 24px; color: #2c3e50; font-weight: bold;")
                          )
                   )
                 ),
                 br(),
                 fluidRow(
                   column(6,
                          highchartOutput("grafico_evolucion_ahorro")
                   ),
                   column(6,
                          highchartOutput("grafico_comparacion_modalidades")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          highchartOutput("grafico_impacto_lagunas")
                   )
                 )
        ),
        tabPanel("Detalle Anual",
                 br(),
                 DTOutput("tabla_detalle")
        ),
        tabPanel("Información",
                 br(),
                 h3("Acerca del Simulador"),
                 p("Este simulador interactivo permite estimar la pensión que una persona podría obtener en el sistema chileno de AFP, considerando factores como edad, ingresos, cotizaciones, lagunas, rentabilidad y tipo de modalidad de pensión."),
                 br(),
                 h3("Consideraciones Importantes"),
                 tags$ul(
                   tags$li("Los cálculos son una aproximación y no deben tomarse como asesoría financiera."),
                   tags$li("El usuario puede especificar su propia esperanza de vida para personalizar los cálculos."),
                   tags$li("La edad de jubilación se establece según el género (60 años mujeres, 65 años hombres) pero puede modificarse."),
                   tags$li("El factor de Renta Vitalicia es personalizable y representa el porcentaje de la pensión de Retiro Programado inicial que ofrece la compañía de seguros."),
                   tags$li("La rentabilidad es hipotética y no garantiza resultados futuros."),
                   tags$li("No considera posibles cambios en la legislación previsional.")
                 ),
                 br(),
                 h3("Metodología"),
                 p("La simulación realiza cálculos año a año considerando el ingreso base, aumentos anuales, cotizaciones, comisiones, rentabilidad y períodos sin cotización."),
                 p("Para el cálculo de la pensión, se divide el saldo acumulado por la expectativa de vida en meses, ajustando según la modalidad elegida.")
        )
      )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Cambiar edad de jubilación según el género seleccionado
  observe({
    if (input$genero == "Hombre") {
      updateNumericInput(session, "edad_jubilacion", value = 65)
    } else {
      updateNumericInput(session, "edad_jubilacion", value = 60)
    }
  })
  
  # Ajustar esperanza de vida según el género (valor inicial sugerido)
  observe({
    if (input$genero == "Hombre") {
      updateNumericInput(session, "esperanza_vida", value = 83)
    } else {
      updateNumericInput(session, "esperanza_vida", value = 89)
    }
  })
  
  # Evento de simulación al hacer clic en el botón
  resultados <- eventReactive(input$simular, {
    # Obtener el factor de renta vitalicia del slider (si está visible)
    factor_rv <- if(!is.null(input$factor_renta_vitalicia)) {
      input$factor_renta_vitalicia / 100  # Convertir de porcentaje a decimal
    } else {
      0.92  # Valor predeterminado si el slider no está visible
    }
    
    resultado <- calcular_pension(
      edad_actual = input$edad_actual, 
      edad_jubilacion = input$edad_jubilacion, 
      esperanza_vida = input$esperanza_vida,
      ingreso_mensual = input$ingreso_mensual, 
      aumento_anual = input$aumento_anual,
      cotizacion = input$cotizacion, 
      comision_afp = as.numeric(input$comision_afp), 
      aporte_empleador = input$aporte_empleador,
      rentabilidad_anual = input$rentabilidad_anual, 
      anos_lagunas = input$anos_lagunas, 
      periodo_lagunas = input$periodo_lagunas
    )
    
    # Actualizar la pensión RV con el factor dinámico
    resultado$pension_mensual_rv <- resultado$pension_mensual_rp * factor_rv
    resultado$tasa_reemplazo_rv <- resultado$tasa_reemplazo_rp * factor_rv
    
    return(resultado)
  })
  
  # Outputs de texto
  output$saldo_acumulado_text <- renderText({
    req(resultados())
    paste0("CLP ", format(round(resultados()$saldo_final), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  output$pension_mensual_text <- renderText({
    req(resultados())
    pension <- if (input$modalidad_pension == "Retiro Programado") {
      resultados()$pension_mensual_rp
    } else {
      resultados()$pension_mensual_rv
    }
    paste0("CLP ", format(round(pension), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  })
  
  output$tasa_reemplazo_text <- renderText({
    req(resultados())
    tasa <- if (input$modalidad_pension == "Retiro Programado") {
      resultados()$tasa_reemplazo_rp
    } else {
      resultados()$tasa_reemplazo_rv
    }
    paste0(round(tasa), "%")
  })
  


  
  # Gráfico de evolución del ahorro
  output$grafico_evolucion_ahorro <- renderHighchart({
    req(resultados())
    df <- resultados()$simulacion
    
    highchart() %>%
      hc_chart(type = "area", backgroundColor = "#f8f9fa") %>%
      hc_title(text = "Evolución del Ahorro Previsional", style = list(fontSize = "18px", fontWeight = "bold")) %>%
      hc_xAxis(
        categories = df$edad,
        title = list(text = "Edad", style = list(fontSize = "14px")),
        labels = list(style = list(fontSize = "12px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Saldo (CLP)", style = list(fontSize = "14px")),
        labels = list(
          formatter = JS("function () { return 'CLP ' + Highcharts.numberFormat(this.value, 0, ',', '.'); }"),
          style = list(fontSize = "12px")
        ),
        gridLineColor = "#e1e1e1"
      ) %>%
      hc_add_series(
        name = "Saldo acumulado",
        data = round(df$saldo_acumulado),
        color = "#0072B2",
        fillOpacity = 0.2,
        marker = list(enabled = TRUE, radius = 3)
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("
        function () {
          return '<b>Edad: ' + this.x + '</b><br>' +
                 'Saldo acumulado: <b>CLP ' + Highcharts.numberFormat(this.y, 0, ',', '.') + '</b>';
        }
      ")
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        area = list(
          lineWidth = 2,
          marker = list(enabled = FALSE),
          fillOpacity = 0.4
        )
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Gráfico de comparación de modalidades
  output$grafico_comparacion_modalidades <- renderHighchart({
    req(resultados())
    
    hc <- highchart() %>%
      hc_chart(type = "column", backgroundColor = "#f8f9fa") %>%
      hc_title(text = "Comparación de Modalidades de Pensión", style = list(fontSize = "18px", fontWeight = "bold")) %>%
      hc_xAxis(
        categories = c("Retiro Programado", "Renta Vitalicia"),
        title = list(text = ""),
        labels = list(style = list(fontSize = "13px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Pensión Mensual (CLP)", style = list(fontSize = "14px")),
        labels = list(
          formatter = JS("function () { return 'CLP ' + Highcharts.numberFormat(this.value, 0, ',', '.'); }"),
          style = list(fontSize = "12px")
        ),
        gridLineColor = "#e1e1e1"
      ) %>%
      hc_add_series(
        name = "Pensión Mensual",
        data = round(c(resultados()$pension_mensual_rp, resultados()$pension_mensual_rv)),
        colorByPoint = TRUE,
        colors = c("#27ae60", "#2980b9")
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("
        function () {
          return '<b>' + this.x + '</b><br>' +
                 'Pensión mensual: <b>CLP ' + Highcharts.numberFormat(this.y, 0, ',', '.') + '</b>';
        }
      ")
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(column = list(
        borderRadius = 4,
        pointPadding = 0.2,
        borderWidth = 0
      )) %>%
      hc_exporting(enabled = TRUE)
    
    return(hc)
  })
  
  # Gráfico de impacto de lagunas
  output$grafico_impacto_lagunas <- renderHighchart({
    req(resultados())
    
    resultados_sin_lagunas <- calcular_pension(
      edad_actual = input$edad_actual, 
      edad_jubilacion = input$edad_jubilacion, 
      esperanza_vida = input$esperanza_vida,
      ingreso_mensual = input$ingreso_mensual, 
      aumento_anual = input$aumento_anual,
      cotizacion = input$cotizacion, 
      comision_afp = as.numeric(input$comision_afp), 
      aporte_empleador = input$aporte_empleador,
      rentabilidad_anual = input$rentabilidad_anual, 
      anos_lagunas = 0, 
      periodo_lagunas = "Aleatorio"
    )
    
    df <- resultados()$simulacion
    df_sin <- resultados_sin_lagunas$simulacion
    
    highchart() %>%
      hc_chart(type = "line", backgroundColor = "#f8f9fa") %>%
      hc_title(text = "Impacto de Lagunas Previsionales", style = list(fontSize = "18px", fontWeight = "bold")) %>%
      hc_xAxis(
        categories = df$edad,
        title = list(text = "Edad", style = list(fontSize = "14px")),
        labels = list(style = list(fontSize = "12px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Saldo (CLP)", style = list(fontSize = "14px")),
        labels = list(
          formatter = JS("function () { return 'CLP ' + Highcharts.numberFormat(this.value, 0, ',', '.'); }"),
          style = list(fontSize = "12px")
        ),
        gridLineColor = "#e1e1e1"
      ) %>%
      hc_add_series(
        name = "Con lagunas",
        data = round(df$saldo_acumulado),
        color = "#e74c3c",
        lineWidth = 3,
        marker = list(enabled = TRUE, radius = 3)
      ) %>%
      hc_add_series(
        name = "Sin lagunas",
        data = round(df_sin$saldo_acumulado),
        color = "#27ae60",
        lineWidth = 3,
        marker = list(enabled = TRUE, radius = 3)
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS("
        function () {
          return '<b>' + this.series.name + '</b><br>' +
                 'Edad: ' + this.x + '<br>' +
                 'Saldo: <b>CLP ' + Highcharts.numberFormat(this.y, 0, ',', '.') + '</b>';
        }
      ")
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        itemStyle = list(fontSize = "13px")
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Tabla de detalle anual
  output$tabla_detalle <- renderDT({
    req(resultados())
    df <- resultados()$simulacion
    
    df_tabla <- df %>%
      mutate(
        Año = ano,
        Edad = edad,
        `Ingreso Mensual` = formatC(round(ingreso_mensual), format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
        `Cotización Anual` = formatC(round(cotizacion_anual), format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
        `Comisión Anual` = formatC(round(comision_anual), format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
        `Rentabilidad` = formatC(round(rentabilidad), format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
        `Saldo Acumulado` = formatC(round(saldo_acumulado), format = "f", big.mark = ".", decimal.mark = ",", digits = 0)
      ) %>%
      select(Año, Edad, `Ingreso Mensual`, `Cotización Anual`, `Comisión Anual`, `Rentabilidad`, `Saldo Acumulado`)
    
    datatable(df_tabla, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json")
    ))
  })
}

# Crear la aplicación Shiny
shinyApp(ui = ui, server = server)