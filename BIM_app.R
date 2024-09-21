library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(shinythemes)
library(shinyjs)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(
    title = "Anñlisis de impacto presupuestario",
    titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introducciñn", tabName = "introduccion", icon = icon("info-circle")),
      menuItem("Poblaciñn diana", tabName = "poblacion_diana", icon = icon("users")),
      menuItem("Costes farmacolñgicos", tabName = "costes_farmacologicos", icon = icon("medkit")),
      menuItem("Cuotas de mercado - Escenario actual", tabName = "cuotas_mercado_actual", icon = icon("chart-pie")),
      menuItem("Cuotas de mercado - Escenario potencial", tabName = "cuotas_mercado_potencial", icon = icon("chart-pie")),
      menuItem("Resultados", tabName = "resultados", icon = icon("table"))
      
    )
  ),
  dashboardBody(
    
    tags$head(tags$style(HTML(
      '
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 15px;
      }
    '))),
    
    tabItems(
      
      # Introduccñn
      tabItem(tabName = "introduccion",
              fluidRow(
                column(12,
                       h2("Introducciñn al Anñlisis de Impacto Presupuestario"),
                       p("Un anñlisis de impacto presupuestario es una herramienta utilizada para evaluar las implicaciones econñmicas de la introducciñn de una nueva intervenciñn de salud, como un medicamento o una tecnologña mñdica, en un sistema de salud."),
                       p("Este anñlisis ayuda a los responsables de la toma de decisiones a entender cñmo los cambios en la prñctica clñnica pueden afectar el presupuesto de salud a corto y largo plazo."),
                       h3("Elementos del Anñlisis de Impacto Presupuestario"),
                       p("La aplicaciñn estñ estructurada en varias secciones que corresponden a los elementos clave de un anñlisis de impacto presupuestario:"),
                       tags$ul(
                         tags$li("Poblaciñn diana: Define la poblaciñn que se verñ afectada por la intervenciñn."),
                         tags$li("Costes farmacolñgicos: Estima los costes asociados a los medicamentos."),
                         tags$li("Cuotas de mercado - Escenario actual: Analiza la distribuciñn actual del mercado."),
                         tags$li("Cuotas de mercado - Escenario potencial: Proyecta la distribuciñn del mercado en un escenario futuro."),
                         tags$li("Resultados: Presenta los resultados del anñlisis, incluyendo el impacto presupuestario.")
                       )
                )
              )
      ),
      
      # Resultados
      tabItem(tabName = "resultados",
              fluidRow(
                column(6,
                       h4("Costes Totales - Escenario Actual"),
                       tableOutput("totalCostTable_actual")
                ),
                column(6,
                       h4("Costes Totales - Escenario Potencial"),
                       tableOutput("totalCostTable_potencial")
                )
              ),
              fluidRow(
                column(12,
                       h4("Impacto Presupuestario"),
                       tableOutput("budgetImpactTable")
                )
              ),
              fluidRow(
                column(12,
                       plotOutput("budgetImpactPlot")
                )
              )
      ),
      
      # Cuotas de mercado - Escenario actual
      tabItem(tabName = "cuotas_mercado_actual",
              fluidRow(
                column(4,
                       h3("Año 1"),
                       numericInput("market_share1_med1_actual", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share1_med2_actual", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market1_actual", "Calcular Año 1")
                ),
                column(4,
                       h3("Año 2"),
                       numericInput("market_share2_med1_actual", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share2_med2_actual", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market2_actual", "Calcular Año 2")
                ),
                column(4,
                       h3("Año 3"),
                       numericInput("market_share3_med1_actual", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share3_med2_actual", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market3_actual", "Calcular Año 3")
                )
              ),
              fluidRow(
                column(6,
                       h4("Cuotas de mercado"),
                       tableOutput("marketShareTable_actual")
                ),
                column(6,
                       h4("Nñmero de pacientes"),
                       tableOutput("patientTable_actual")
                )
              ),
              fluidRow(
                column(12,
                       plotOutput("marketSharePlot_actual")
                )
              )
      ),
      # Cuotas de mercado - Escenario potencial
      tabItem(tabName = "cuotas_mercado_potencial",
              fluidRow(
                column(4,
                       h3("Año 1"),
                       numericInput("market_share1_med1_potencial", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share1_med2_potencial", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market1_potencial", "Calcular Año 1")
                ),
                column(4,
                       h3("Año 2"),
                       numericInput("market_share2_med1_potencial", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share2_med2_potencial", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market2_potencial", "Calcular Año 2")
                ),
                column(4,
                       h3("Año 3"),
                       numericInput("market_share3_med1_potencial", "Cuota de mercado Medicamento 1 (%):", value = 50),
                       numericInput("market_share3_med2_potencial", "Cuota de mercado Medicamento 2 (%):", value = 50),
                       actionButton("calculate_market3_potencial", "Calcular Año 3")
                )
              ),
              fluidRow(
                column(6,
                       h4("Cuotas de mercado"),
                       tableOutput("marketShareTable_potencial")
                ),
                column(6,
                       h4("Nñmero de pacientes"),
                       tableOutput("patientTable_potencial")
                )
              ),
              fluidRow(
                column(12,
                       plotOutput("marketSharePlot_potencial")
                )
              )
      ),
      # Poblaciñn diana
      tabItem(tabName = "poblacion_diana",
              fluidRow(
                column(4,
                       numericInput("total_population_year1", "Poblaciñn Total Año 1:", value = 1000),
                       numericInput("aware_year1", "Conocimiento del Estado Año 1 (%):", value = 80),
                       numericInput("linked_year1", "Vinculados a Servicios Año 1 (%):", value = 70),
                       numericInput("retained_year1", "Retenidos en Servicios Año 1 (%):", value = 60),
                       numericInput("treated_year1", "En Tratamiento Año 1 (%):", value = 50),
                       numericInput("suppressed_year1", "Supresiñn Viral Año 1 (%):", value = 40),
                       actionButton("calculate_year1", "Calcular Año 1")
                ),
                column(4,
                       numericInput("total_population_year2", "Poblaciñn Total Año 2:", value = 1000),
                       numericInput("aware_year2", "Conocimiento del Estado Año 2 (%):", value = 80),
                       numericInput("linked_year2", "Vinculados a Servicios Año 2 (%):", value = 70),
                       numericInput("retained_year2", "Retenidos en Servicios Año 2 (%):", value = 60),
                       numericInput("treated_year2", "En Tratamiento Año 2 (%):", value = 50),
                       numericInput("suppressed_year2", "Supresiñn Viral Año 2 (%):", value = 40),
                       actionButton("calculate_year2", "Calcular Año 2")
                ),
                column(4,
                       numericInput("total_population_year3", "Poblaciñn Total Año 3:", value = 1000),
                       numericInput("aware_year3", "Conocimiento del Estado Año 3 (%):", value = 80),
                       numericInput("linked_year3", "Vinculados a Servicios Año 3 (%):", value = 70),
                       numericInput("retained_year3", "Retenidos en Servicios Año 3 (%):", value = 60),
                       numericInput("treated_year3", "En Tratamiento Año 3 (%):", value = 50),
                       numericInput("suppressed_year3", "Supresiñn Viral Año 3 (%):", value = 40),
                       actionButton("calculate_year3", "Calcular Año 3")
                )
              ),
              fluidRow(
                column(12,
                       plotOutput("cascadePlot")
                )
              ),
              fluidRow(
                column(12,
                       tableOutput("cascadeTable")
                )
              )
      ),
      # Costes farmacolñgicos
      tabItem(tabName = "costes_farmacologicos",
              fluidRow(
                column(6,
                       h3("Medicamento 1"),
                       numericInput("dose1", "Dosis por administraciñn:", value = 50),
                       numericInput("admin_per_cycle1", "Nñm. administraciones por ciclo:", value = 2),
                       numericInput("cycle_duration1", "Duraciñn del ciclo (dñas):", value = 7),
                       numericInput("treatment_duration1", "Duraciñn del tratamiento (dñas):", value = 28),
                       numericInput("mg_per_presentation1", "Miligramos por presentaciñn:", value = 100),
                       numericInput("price_per_presentation1", "Precio presentaciñn:", value = 20),
                       actionButton("calculate_cost1", "Calcular coste"),
                       tableOutput("costTable1")
                ),
                column(6,
                       h3("Medicamento 2"),
                       numericInput("dose2", "Dosis por administraciñn:", value = 50),
                       numericInput("admin_per_cycle2", "Nñm. administraciones por ciclo:", value = 2),
                       numericInput("cycle_duration2", "Duraciñn del ciclo (dñas):", value = 7),
                       numericInput("treatment_duration2", "Duraciñn del tratamiento (dñas):", value = 28),
                       numericInput("mg_per_presentation2", "Miligramos por presentaciñn:", value = 100),
                       numericInput("price_per_presentation2", "Precio presentaciñn:", value = 20),
                       actionButton("calculate_cost2", "Calcular coste"),
                       tableOutput("costTable2")
                )
              )
      )
    )
  )
)
# Definir la lñgica del servidor
server <- function(input, output) {
  
  cascade_data           <- reactiveValues(data = NULL)
  market_data_actual     <- reactiveValues(data = NULL)
  market_data_potencial  <- reactiveValues(data = NULL)
  patient_data_actual    <- reactiveValues(data = NULL)
  patient_data_potencial <- reactiveValues(data = NULL)
  
  # Calcular costes totales
  calculate_total_costs <- function(patient_data, cost_per_patient) {
    total_costs <- patient_data * cost_per_patient
    total_costs
  }
  
  # Costes totales - Escenario actual
  total_costs_actual <- reactive({
    if (!is.null(patient_data_actual$data) && !is.null(cascade_data$data)) {
      cost_per_patient1 <- input$price_per_presentation1 * (input$dose1 / input$mg_per_presentation1) * (input$treatment_duration1 / input$cycle_duration1) * input$admin_per_cycle1
      cost_per_patient2 <- input$price_per_presentation2 * (input$dose2 / input$mg_per_presentation2) * (input$treatment_duration2 / input$cycle_duration2) * input$admin_per_cycle2
      
      total_costs1 <- calculate_total_costs(patient_data_actual$data[1, -1], cost_per_patient1)
      total_costs2 <- calculate_total_costs(patient_data_actual$data[2, -1], cost_per_patient2)
      
      total_costs <- rbind(total_costs1, total_costs2)
      total_costs <- rbind(total_costs, colSums(total_costs))
      rownames(total_costs) <- c("Medicamento 1", "Medicamento 2", "Coste Total")
      
      total_costs
    }
  })
  
  output$totalCostTable_actual <- renderTable({
    total_costs_actual()
  }, rownames = TRUE)
  
  # Costes totales - Escenario potencial
  total_costs_potencial <- reactive({
    if (!is.null(patient_data_potencial$data) && !is.null(cascade_data$data)) {
      cost_per_patient1 <- input$price_per_presentation1 * (input$dose1 / input$mg_per_presentation1) * (input$treatment_duration1 / input$cycle_duration1) * input$admin_per_cycle1
      cost_per_patient2 <- input$price_per_presentation2 * (input$dose2 / input$mg_per_presentation2) * (input$treatment_duration2 / input$cycle_duration2) * input$admin_per_cycle2
      
      total_costs1 <- calculate_total_costs(patient_data_potencial$data[1, -1], cost_per_patient1)
      total_costs2 <- calculate_total_costs(patient_data_potencial$data[2, -1], cost_per_patient2)
      
      total_costs <- rbind(total_costs1, total_costs2)
      total_costs <- rbind(total_costs, colSums(total_costs))
      rownames(total_costs) <- c("Medicamento 1", "Medicamento 2", "Coste Total")
      
      total_costs
    }
  })
  
  output$totalCostTable_potencial <- renderTable({
    total_costs_potencial()
  }, rownames = TRUE)
  
  # Impacto presupuestario
  budget_impact <- reactive({
    if (!is.null(total_costs_actual()) && !is.null(total_costs_potencial())) {
      actual_costs <- total_costs_actual()[3, ]
      potencial_costs <- total_costs_potencial()[3, ]
      
      budget_impact <- rbind(actual_costs, potencial_costs, potencial_costs - actual_costs)
      rownames(budget_impact) <- c("Escenario Actual", "Escenario Potencial", "Diferencia")
      
      budget_impact
    }
  })
  
  output$budgetImpactTable <- renderTable({
    budget_impact()
  }, rownames = TRUE)
  
  output$budgetImpactPlot <- renderPlot({
    if (!is.null(budget_impact())) {
      budget_impact_data <- as.data.frame(t(budget_impact()))
      budget_impact_data$Scenario <- rownames(budget_impact())
      budget_impact_data <- tidyr::gather(budget_impact_data, key = "Year", value = "Cost", -Scenario)
      
      # Asegurarse de que los años sean factores para mantener el orden correcto
      budget_impact_data$Year <- factor(budget_impact_data$Year, levels = unique(budget_impact_data$Year))
      
      ggplot(budget_impact_data, aes(x = Year, y = Cost, fill = Scenario)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
        scale_fill_manual(values = c("blue", "green", "red")) +
        labs(title = "Impacto Presupuestario", x = "Años", y = "Coste Total") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Cuotas de mercado - Escenario actual
  
  observeEvent(input$calculate_market1_actual, {
    market_share1_med1_actual <- input$market_share1_med1_actual / 100
    market_share1_med2_actual <- input$market_share1_med2_actual / 100
    total_population_year1 <- if (!is.null(cascade_data$data)) cascade_data$data$Año1[1] else 0
    
    patients_med1_year1_actual <- total_population_year1 * market_share1_med1_actual
    patients_med2_year1_actual <- total_population_year1 * market_share1_med2_actual
    
    market_data_actual$data <- data.frame(
      Medicamento = c("Medicamento 1", "Medicamento 2"),
      `Año 1` = c(market_share1_med1_actual * 100, market_share1_med2_actual * 100)
    )
    
    patient_data_actual$data <- data.frame(
      Medicamento = c("Medicamento 1", "Medicamento 2"),
      `Año 1` = c(patients_med1_year1_actual, patients_med2_year1_actual)
    )
  })
  
  observeEvent(input$calculate_market2_actual, {
    market_share2_med1_actual <- input$market_share2_med1_actual / 100
    market_share2_med2_actual <- input$market_share2_med2_actual / 100
    total_population_year2 <- if (!is.null(cascade_data$data)) cascade_data$data$Año2[1] else 0
    
    patients_med1_year2_actual <- total_population_year2 * market_share2_med1_actual
    patients_med2_year2_actual <- total_population_year2 * market_share2_med2_actual
    
    market_data_actual$data <- cbind(market_data_actual$data, `Año 2` = c(market_share2_med1_actual * 100, market_share2_med2_actual * 100))
    patient_data_actual$data <- cbind(patient_data_actual$data, `Año 2` = c(patients_med1_year2_actual, patients_med2_year2_actual))
  })
  
  observeEvent(input$calculate_market3_actual, {
    market_share3_med1_actual <- input$market_share3_med1_actual / 100
    market_share3_med2_actual <- input$market_share3_med2_actual / 100
    total_population_year3 <- if (!is.null(cascade_data$data)) cascade_data$data$Año3[1] else 0
    
    patients_med1_year3_actual <- total_population_year3 * market_share3_med1_actual
    patients_med2_year3_actual <- total_population_year3 * market_share3_med2_actual
    
    market_data_actual$data <- cbind(market_data_actual$data, `Año 3` = c(market_share3_med1_actual * 100, market_share3_med2_actual * 100))
    patient_data_actual$data <- cbind(patient_data_actual$data, `Año 3` = c(patients_med1_year3_actual, patients_med2_year3_actual))
  })
  
  output$marketShareTable_actual <- renderTable({
    if (!is.null(market_data_actual$data)) {
      market_data_actual$data
    }
  })
  
  output$patientTable_actual <- renderTable({
    if (!is.null(patient_data_actual$data)) {
      patient_data_actual$data
    }
  })
  
  output$marketSharePlot_actual <- renderPlot({
    if (!is.null(market_data_actual$data)) {
      bar_data <- as.matrix(market_data_actual$data[, -1])
      barplot(bar_data, beside = FALSE, col = c("blue", "red"), legend = rownames(bar_data), xlab = "Años", ylab = "Cuota de mercado (%)", names.arg = colnames(bar_data))
    }
  })
  
  # Cuotas de mercado - Escenario potencial
  
  observeEvent(input$calculate_market1_potencial, {
    market_share1_med1_potencial <- input$market_share1_med1_potencial / 100
    market_share1_med2_potencial <- input$market_share1_med2_potencial / 100
    total_population_year1 <- if (!is.null(cascade_data$data)) cascade_data$data$Año1[1] else 0
    
    patients_med1_year1_potencial <- total_population_year1 * market_share1_med1_potencial
    patients_med2_year1_potencial <- total_population_year1 * market_share1_med2_potencial
    
    market_data_potencial$data <- data.frame(
      Medicamento = c("Medicamento 1", "Medicamento 2"),
      `Año 1` = c(market_share1_med1_potencial * 100, market_share1_med2_potencial * 100)
    )
    
    patient_data_potencial$data <- data.frame(
      Medicamento = c("Medicamento 1", "Medicamento 2"),
      `Año 1` = c(patients_med1_year1_potencial, patients_med2_year1_potencial)
    )
  })
  
  observeEvent(input$calculate_market2_potencial, {
    market_share2_med1_potencial <- input$market_share2_med1_potencial / 100
    market_share2_med2_potencial <- input$market_share2_med2_potencial / 100
    total_population_year2 <- if (!is.null(cascade_data$data)) cascade_data$data$Año2[1] else 0
    
    patients_med1_year2_potencial <- total_population_year2 * market_share2_med1_potencial
    patients_med2_year2_potencial <- total_population_year2 * market_share2_med2_potencial
    
    market_data_potencial$data <- cbind(market_data_potencial$data, `Año 2` = c(market_share2_med1_potencial * 100, market_share2_med2_potencial * 100))
    patient_data_potencial$data <- cbind(patient_data_potencial$data, `Año 2` = c(patients_med1_year2_potencial, patients_med2_year2_potencial))
  })
  
  observeEvent(input$calculate_market3_potencial, {
    market_share3_med1_potencial <- input$market_share3_med1_potencial / 100
    market_share3_med2_potencial <- input$market_share3_med2_potencial / 100
    total_population_year3 <- if (!is.null(cascade_data$data)) cascade_data$data$Año3[1] else 0
    
    patients_med1_year3_potencial <- total_population_year3 * market_share3_med1_potencial
    patients_med2_year3_potencial <- total_population_year3 * market_share3_med2_potencial
    
    market_data_potencial$data <- cbind(market_data_potencial$data, `Año 3` = c(market_share3_med1_potencial * 100, market_share3_med2_potencial * 100))
    patient_data_potencial$data <- cbind(patient_data_potencial$data, `Año 3` = c(patients_med1_year3_potencial, patients_med2_year3_potencial))
  })
  
  output$marketShareTable_potencial <- renderTable({
    if (!is.null(market_data_potencial$data)) {
      market_data_potencial$data
    }
  })
  
  output$patientTable_potencial <- renderTable({
    if (!is.null(patient_data_potencial$data)) {
      patient_data_potencial$data
    }
  })
  
  output$marketSharePlot_potencial <- renderPlot({
    if (!is.null(market_data_potencial$data)) {
      bar_data <- as.matrix(market_data_potencial$data[, -1])
      barplot(bar_data, beside = FALSE, col = c("blue", "red"), legend = rownames(bar_data), xlab = "Años", ylab = "Cuota de mercado (%)", names.arg = colnames(bar_data))
    }
  })
  
  # Poblaciñn diana
  
  observeEvent(input$calculate_year1, {
    total_population_year1 <- input$total_population_year1
    aware_year1 <- total_population_year1 * (input$aware_year1 / 100)
    linked_year1 <- aware_year1 * (input$linked_year1 / 100)
    retained_year1 <- linked_year1 * (input$retained_year1 / 100)
    treated_year1 <- retained_year1 * (input$treated_year1 / 100)
    suppressed_year1 <- treated_year1 * (input$suppressed_year1 / 100)
    
    cascade_data$data <- data.frame(
      Stage = c("Supresiñn Viral", "En Tratamiento", "Retenidos en Servicios", "Vinculados a Servicios", "Conocimiento del Estado", "Total Poblaciñn"),
      Año1 = c(suppressed_year1, treated_year1, retained_year1, linked_year1, aware_year1, total_population_year1),
      Año2 = if (!is.null(cascade_data$data)) cascade_data$data$Año2 else NA,
      Año3 = if (!is.null(cascade_data$data)) cascade_data$data$Año3 else NA
    )
  })
  
  observeEvent(input$calculate_year2, {
    total_population_year2 <- input$total_population_year2
    aware_year2 <- total_population_year2 * (input$aware_year2 / 100)
    linked_year2 <- aware_year2 * (input$linked_year2 / 100)
    retained_year2 <- linked_year2 * (input$retained_year2 / 100)
    treated_year2 <- retained_year2 * (input$treated_year2 / 100)
    suppressed_year2 <- treated_year2 * (input$suppressed_year2 / 100)
    
    cascade_data$data <- data.frame(
      Stage = c("Supresiñn Viral", "En Tratamiento", "Retenidos en Servicios", "Vinculados a Servicios", "Conocimiento del Estado", "Total Poblaciñn"),
      Año1 = if (!is.null(cascade_data$data)) cascade_data$data$Año1 else NA,
      Año2 = c(suppressed_year2, treated_year2, retained_year2, linked_year2, aware_year2, total_population_year2),
      Año3 = if (!is.null(cascade_data$data)) cascade_data$data$Año3 else NA
    )
  })
  
  observeEvent(input$calculate_year3, {
    total_population_year3 <- input$total_population_year3
    aware_year3 <- total_population_year3 * (input$aware_year3 / 100)
    linked_year3 <- aware_year3 * (input$linked_year3 / 100)
    retained_year3 <- linked_year3 * (input$retained_year3 / 100)
    treated_year3 <- retained_year3 * (input$treated_year3 / 100)
    suppressed_year3 <- treated_year3 * (input$suppressed_year3 / 100)
    
    cascade_data$data <- data.frame(
      Stage = c("Supresiñn Viral", "En Tratamiento", "Retenidos en Servicios", "Vinculados a Servicios", "Conocimiento del Estado", "Total Poblaciñn"),
      Año1 = if (!is.null(cascade_data$data)) cascade_data$data$Año1 else NA,
      Año2 = if (!is.null(cascade_data$data)) cascade_data$data$Año2 else NA,
      Año3 = c(suppressed_year3, treated_year3, retained_year3, linked_year3, aware_year3, total_population_year3)
    )
  })
  
  output$cascadePlot <- renderPlot({
    if (!is.null(cascade_data$data)) {
      barplot(cascade_data$data$Año1, names.arg = cascade_data$data$Stage, col = "skyblue", horiz = TRUE, las = 1, main = "Cascada Poblacional Año 1", xlab = "Nñmero de Personas")
      abline(v = 0, col = "red", lwd = 2)  # Eje central
    }
  })
  
  output$cascadeTable <- renderTable({
    if (!is.null(cascade_data$data)) {
      cascade_data$data[order(nrow(cascade_data$data):1), ]  # Invertir el orden de las filas
    }
  })
  
  # Costes farmacolñgicos
  
  observeEvent(input$calculate_cost1, {
    dose1 <- input$dose1
    admin_per_cycle1 <- input$admin_per_cycle1
    cycle_duration1 <- input$cycle_duration1
    treatment_duration1 <- input$treatment_duration1
    mg_per_presentation1 <- input$mg_per_presentation1
    price_per_presentation1 <- input$price_per_presentation1
    
    total_doses1 <- (treatment_duration1 / cycle_duration1) * admin_per_cycle1
    total_mg1 <- total_doses1 * dose1
    boxes_per_dose1 <- dose1 / mg_per_presentation1
    total_boxes1 <- boxes_per_dose1 * total_doses1
    total_cost1 <- total_boxes1 * price_per_presentation1
    
    output$costTable1 <- renderTable({
      data.frame(
        Variable = c("Dosis por administraciñn", "Nñm. administraciones por ciclo", "Duraciñn del ciclo (dñas)", "Duraciñn del tratamiento (dñas)", "Dosis total", "Miligramos por presentaciñn", "Nñm. cajas necesarias por dosis", "Precio presentaciñn", "Cajas por tratamiento", "Coste tratamiento total"),
        Valor = c(dose1, admin_per_cycle1, cycle_duration1, treatment_duration1, total_mg1, mg_per_presentation1, boxes_per_dose1, price_per_presentation1, total_boxes1, total_cost1)
      )
    })
  })
  
  observeEvent(input$calculate_cost2, {
    dose2 <- input$dose2
    admin_per_cycle2 <- input$admin_per_cycle2
    cycle_duration2 <- input$cycle_duration2
    treatment_duration2 <- input$treatment_duration2
    mg_per_presentation2 <- input$mg_per_presentation2
    price_per_presentation2 <- input$price_per_presentation2
    
    total_doses2 <- (treatment_duration2 / cycle_duration2) * admin_per_cycle2
    total_mg2 <- total_doses2 * dose2
    boxes_per_dose2 <- dose2 / mg_per_presentation2
    total_boxes2 <- boxes_per_dose2 * total_doses2
    total_cost2 <- total_boxes2 * price_per_presentation2
    
    output$costTable2 <- renderTable({
      data.frame(
        Variable = c("Dosis por administraciñn", "Nñm. administraciones por ciclo", "Duraciñn del ciclo (dñas)", "Duraciñn del tratamiento (dñas)", "Dosis total", "Miligramos por presentaciñn", "Nñm. cajas necesarias por dosis", "Precio presentaciñn", "Cajas por tratamiento", "Coste tratamiento total"),
        Valor = c(dose2, admin_per_cycle2, cycle_duration2, treatment_duration2, total_mg2, mg_per_presentation2, boxes_per_dose2, price_per_presentation2, total_boxes2, total_cost2)
      )
    })
  })
}

# Ejecutar la aplicaciñn
shinyApp(ui = ui, server = server)
