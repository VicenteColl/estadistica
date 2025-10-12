library(shiny)
library(ggplot2)
library(dplyr)

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  # Logo y botón de cierre en la parte superior derecha
  # div(style = "position: absolute; right: 20px; top: 10px; z-index: 1000;",
  #     a(href = 'https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ',
  #       target = '_blank',
  #       img(src = 'logo.jpg',
  #           title = 'Canal youtube:\nLa magia de estadistica',
  #           height = "60px",
  #           style = "margin-right: 15px;")),
  #     a(
  #       href = "javascript:(function(){
  #   window.location.href='https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ';
  #   setTimeout(function(){ window.close(); }, 500);
  # })()",
  #       title = "Cerrar aplicación",
  #       style = "text-decoration: none; margin-left: 10px; text-align: center; display: inline-block;",
  #       tagList(
  #         icon("power-off", style = "font-size: 30px; color: #d9534f; vertical-align: middle;"),
  #         tags$div("Cerrar app", style = "font-size: 12px; color: #333; margin-top: 3px;")
  #       )
  #     )
  # ),
  # --- BLOQUE REEMPLAZO CABECERA (poner en ui en lugar del div anterior) ---
  tags$head(
    tags$style(HTML('
    /* header */
    .app-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 8px 20px;
      position: relative;
      z-index: 1000;
      background: transparent; /* cambiar si quieres fondo */
    }
    /* marca (logo + titulo) */
    .brand {
      display: flex;
      align-items: center;
      gap: 12px;
    }
    .brand img {
      height: 60px; /* tu logo 60px */
      display: block;
    }
    .app-title {
      font-size: 30px;
      font-weight: 600;
      margin: 0;
      line-height: 1;
    }
    /* boton cerrar a la derecha */
    .close-wrapper {
      display: flex;
      flex-direction: column;
      align-items: center;
      text-decoration: none;
      color: inherit;
    }
    .close-wrapper .icon {
      font-size: 30px; /* tamaño del icono */
      color: #d9534f;
      display: block;
      margin-bottom: 4px; /* separacion con el texto */
      position: relative;
      top: 2px; /* ajusta para bajar/subir el icono */
    }
    .close-wrapper .label {
      font-size: 12px;
      color: #333;
    }

    /* pequeño ajuste respuesta en pantallas pequeñas */
    @media (max-width: 480px) {
      .app-title { font-size: 16px; }
      .brand img { height: 48px; }
    }
  '))
  ),

  div(class = "app-header",
      div(class = "brand",
          a(href = 'https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ',
            target = '_blank',
            img(src = 'logo.jpg',
                title = 'Canal youtube:\\nLa magia de estadistica',
                height = "60px",
                style = "display:block;")
          ),
          # Título al lado del logo
          tags$div(
            tags$h1("Aprendizaje de Distribuciones de Probabilidad", class = "app-title")
          )
      ),

      # Botón cerrar a la derecha (redirecciona y luego intenta cerrar ventana)
      a(href = "javascript:(function(){
                 window.location.href='https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ';
                 setTimeout(function(){ window.close(); }, 500);
               })()",
        title = "Cerrar aplicación",
        class = "close-wrapper",
        # icon() produce etiquetas <i>, las envolvemos para aplicar estilo
        tags$span(class = "icon", icon("power-off")),
        tags$div(class = "label", "Cerrar app")
      )
  ),
  # --- FIN BLOQUE REEMPLAZO CABECERA ---

  # titlePanel("Aprendizaje de Distribuciones de Probabilidad"),
  uiOutput("mainUI")
)


server <- function(input, output, session) {
  # Inicializar valores reactivos
  mostrar_ejercicios <- reactiveVal(0)
  contador_ayuda <- reactiveVal(0)

  # Controlar la visualización del sidebar y main panel
  output$mainUI <- renderUI({
    if (mostrar_ejercicios() == 0) {
      sidebarLayout(
        sidebarPanel(
          radioButtons("tipo_dist", "Tipo de distribución:",
                       choices = c("Discreta", "Continua"), selected = "Discreta"),
          selectInput("distribucion", "Seleccione distribución:",
                      choices = c("Binomial", "Poisson", "Normal", "Uniforme Discreta",
                                  "Binomial Negativa", "Hipergeométrica", "t-Student",
                                  "Chi-cuadrado", "F", "Exponencial")),
          uiOutput("parametros"),


          conditionalPanel(
            condition = "input.tipo_calculo != 'cuantil'",
            numericInput("valor_x", "Valor de x:", value = 0)
          ),
          # Mostrar las opciones de tipo de cálculo según si la distribución es discreta o continua
          uiOutput("tipoCalculoUI"),
          conditionalPanel(
            condition = "input.tipo_calculo == 'cuantil'",
            sliderInput("prob_p", "Probabilidad p:", min = 0, max = 1,
                        value = 0.5, step = 0.01),
            numericInput("prob_p_num", "O ingrese p numéricamente:",
                         min = 0, max = 1, value = 0.5, step = 0.01)
          ),
          actionButton("calcular", "Calcular"),
          hr(),
          h4("Resultado:"),
          verbatimTextOutput("resultado"),
          hr(),
          h4("Explicación:"),
          uiOutput("explicacion")
        ),
        mainPanel(
          plotOutput("grafico", height = "400px"),
          conditionalPanel(
            condition = "input.tipo_calculo != 'puntual'",
            checkboxInput("mostrar_area", "Mostrar área de probabilidad", value = TRUE)
          ),
          div(style = "text-align: center;",
              actionButton("mostrar_ejercicios", "EJERCICIOS PARA PRACTICAR",
                           style = "margin-top: 20px; width: 40%;"))
        )
      )
    } else {
      # Pantalla completa para ejercicios
      fluidPage(
        h3("Ejercicio de Práctica"),
        uiOutput("enunciado_ejercicio"),
        div(style = "width: 200px; margin-bottom: 15px;",
            numericInput("respuesta_usuario", "Tu respuesta:",
                         value = NA, min = 0, max = 1, step = 0.001,
                         width = "100%")),
        tags$script(HTML(
          "$(document).on('keyup', '#respuesta_usuario', function() {
            var value = $(this).val().replace(',', '.');
            if(value !== $(this).val()) {
              $(this).val(value);
            }
          });"
        )),
        fluidRow(
          column(6, actionButton("verificar_respuesta", "Comprobar respuesta",
                                 style = "width: 100%; margin-bottom: 15px;")),
          column(6, actionButton("ayuda_grafica", "Ayuda gráfica",
                                 style = "width: 100%; background-color: #d4edda;
                                          margin-bottom: 15px;"))
        ),
        htmlOutput("retroalimentacion"),
        conditionalPanel(
          condition = "output.mostrar_grafico_condicional == true",
          plotOutput("grafico_ejercicio", height = "300px")
        ),
        fluidRow(
          column(6,
                 fluidRow(
                   column(6, actionButton("volver_graficos", "ATRÁS",
                                          style = "width: 100%; background-color: #f8f9fa;
                                                   border: 1px solid #ddd;")),
                   column(6, actionButton("nuevo_ejercicio", "NUEVO EJERCICIO",
                                          style = "width: 100%; background-color: #4CAF50;
                                                   color: white;"))
                 ),
                 column(6, div())
          )
        )
      )
    }
  })

  # Asegurar que el valor máximo sea siempre mayor que el mínimo en la Uniforme Discreta
  observeEvent(input$min_unif, {
    if (!is.null(input$max_unif) && input$max_unif <= input$min_unif) {
      updateNumericInput(session, "max_unif", value = input$min_unif + 1)
    }
  })

  observeEvent(input$max_unif, {
    if (!is.null(input$min_unif) && input$max_unif <= input$min_unif) {
      updateNumericInput(session, "min_unif", value = input$max_unif - 1)
    }
  })


  # Output condicional para mostrar el gráfico
  output$mostrar_grafico_condicional <- reactive({
    contador_ayuda() %% 2 == 1
  })
  outputOptions(output, "mostrar_grafico_condicional", suspendWhenHidden = FALSE)

  # Gráfico por defecto (Binomial)
  output$grafico <- renderPlot({
    datos <- data.frame(
      x = 0:10,
      y = dbinom(0:10, size = 10, prob = 0.5)
    )
    ggplot(datos, aes(x = x, y = y)) +
      geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
      geom_point(size = 3, color = "navy") +
      theme_minimal() +
      labs(title = "Distribución Binomial (n=10, p=0.5)",
           x = "Valor", y = "Probabilidad")
  })


  # Actualizar automáticamente el gráfico al cambiar de distribución o parámetros
  observe({
    req(input$distribucion)
    isolate({
      resultado_temp <- list(valor = 0, prob = 0, tipo = "puntual", texto = "")
      output$grafico <- renderPlot({
        graficar_distribucion(resultado_temp)
      })
    })
  })

  observeEvent(input$mostrar_ejercicios, {
    mostrar_ejercicios(1)
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})
    generar_ejercicio()
  })

  observeEvent(input$volver_graficos, {
    mostrar_ejercicios(0)
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})
  })

  observeEvent(input$nuevo_ejercicio, {
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})
    updateNumericInput(session, "respuesta_usuario", value = NA)
    generar_ejercicio()
  })

  observeEvent(input$ayuda_grafica, {
    contador_ayuda(contador_ayuda() + 1)
  })

  # Actualizar las distribuciones disponibles según el tipo seleccionado
  observeEvent(input$tipo_dist, {
    if (input$tipo_dist == "Discreta") {
      updateSelectInput(session, "distribucion",
                        choices = c("Binomial", "Poisson", "Uniforme Discreta",
                                    "Binomial Negativa", "Hipergeométrica"))
    } else {
      updateSelectInput(session, "distribucion",
                        choices = c("Normal", "t-Student", "Chi-cuadrado",
                                    "F", "Exponencial"))
    }
  })

  # nuevo bloque
  # Renderizar dinámicamente las opciones del tipo de cálculo según el tipo de distribución
  output$tipoCalculoUI <- renderUI({
    if (input$tipo_dist == "Discreta") {
      radioButtons("tipo_calculo", "Tipo de cálculo:",
                   choices = c("Probabilidad puntual (P(X = x))" = "puntual",
                               "Probabilidad acumulada (P(X ≤ x))" = "acumulada",
                               "Probabilidad mayor que (P(X > x))" = "mayor",
                               "Cuantil (P(X ≤ q) = p)" = "cuantil"),
                   selected = "puntual")
    } else {
      radioButtons("tipo_calculo", "Tipo de cálculo:",
                   choices = c("Probabilidad acumulada (P(X ≤ x))" = "acumulada",
                               "Probabilidad mayor que (P(X > x))" = "mayor",
                               "Cuantil (P(X ≤ q) = p)" = "cuantil"),
                   selected = "acumulada")
    }
  })
  # fin nuevo bloque

  # Sincronizar los inputs de probabilidad para cuantiles
  observeEvent(input$prob_p, {
    updateNumericInput(session, "prob_p_num", value = input$prob_p)
  })

  observeEvent(input$prob_p_num, {
    updateSliderInput(session, "prob_p", value = input$prob_p_num)
  })


  # Actualizar parámetros según la distribución seleccionada
  output$parametros <- renderUI({
    tagList(
      switch(input$distribucion,
             "Binomial" = {
               tagList(
                 numericInput("n", "Número de ensayos (n):", value = 10, min = 1),
                 numericInput("p", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1, step=0.1)
               )
             },
             "Poisson" = {
               numericInput("lambda", "Tasa (λ):", value = 1, min = 0, max=10, step = 0.5)
             },
             "Normal" = {
               tagList(
                 numericInput("media", "Media (μ):", value = 0),
                 numericInput("sd", "Desviación estándar (σ):", value = 1, min = 0.01)
               )
             },
             "Uniforme Discreta" = {
               tagList(
                 numericInput("min_unif", "Mínimo (a):", value = 0),
                 numericInput("max_unif", "Máximo (b):", value = 10, min = 1)
               )
             },
             "Binomial Negativa" = {
               tagList(
                 numericInput("r", "Número de éxitos (r):", value = 5, min = 1),
                 numericInput("p_neg", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1)
               )
             },
             "Hipergeométrica" = {
               tagList(
                 numericInput("m", "Número de éxitos en población (m):", value = 10, min = 1),
                 numericInput("n_hip", "Número de fracasos en población (n):", value = 10, min = 1),
                 numericInput("k", "Número de extracciones (k):", value = 5, min = 1)
               )
             },
             "t-Student" = {
               numericInput("df_t", "Grados de libertad (df):", value = 5, min = 1)
             },
             "Chi-cuadrado" = {
               numericInput("df_chi", "Grados de libertad (df):", value = 5, min = 1)
             },
             "F" = {
               tagList(
                 numericInput("df1", "Grados de libertad numerador (df1):", value = 5, min = 1),
                 numericInput("df2", "Grados de libertad denominador (df2):", value = 10, min = 1)
               )
             },
             "Exponencial" = {
               numericInput("rate", "Tasa (λ):", value = 1, min = 0.01)
             }
      )
    )
  })

  # Generar explicación teórica
  output$explicacion <- renderUI({
    explicacion <- switch(input$distribucion,
                          "Binomial" = "La distribución binomial modela el número de éxitos en n ensayos independientes con probabilidad p.",
                          "Poisson" = "La distribución Poisson modela eventos raros en un intervalo de tiempo/espacio con tasa λ.",
                          "Normal" = "Distribución continua simétrica alrededor de su media μ con desviación σ.",
                          "Uniforme Discreta" = "Todos los resultados entre a y b tienen igual probabilidad.",
                          "Binomial Negativa" = "Número de ensayos hasta obtener r éxitos con probabilidad p.",
                          "Hipergeométrica" = "Éxitos en k extracciones sin reemplazo de población finita.",
                          "t-Student" = "Similar a la normal pero con colas más pesadas, usada en muestras pequeñas.",
                          "Chi-cuadrado" = "Suma de variables normales estándar al cuadrado. Usada en tests de hipótesis.",
                          "F" = "Razón de dos variables chi-cuadrado. Usada en ANOVA.",
                          "Exponencial" = "Modela tiempos entre eventos en un proceso de Poisson con tasa λ."
    )

    HTML(paste("<p>", explicacion, "</p>"))
  })

  # Calcular y graficar cuando se presiona el botón
  observeEvent(input$calcular, {
    req(input$distribucion)

    # Calcular según el tipo de operación
    resultado <- switch(input$tipo_calculo,
                        "puntual" = calcular_puntual(),
                        "acumulada" = calcular_acumulada(),
                        "mayor" = calcular_mayor(),
                        "cuantil" = calcular_cuantil()
    )

    output$resultado <- renderPrint({
      resultado$texto
    })

    output$grafico <- renderPlot({
      graficar_distribucion(resultado)
    })
  })

  # Funciones de cálculo -----
  calcular_puntual <- function() {
    valor <- input$valor_x
    prob <- switch(input$distribucion,
                   "Binomial" = dbinom(valor, input$n, input$p),
                   "Poisson" = dpois(valor, input$lambda),
                   "Normal" = dnorm(valor, input$media, input$sd),
                   "Uniforme Discreta" = ifelse(valor >= input$min_unif & valor <= input$max_unif,
                                                1/(input$max_unif - input$min_unif + 1), 0),
                   "Binomial Negativa" = dnbinom(valor - input$r, input$r, input$p_neg),
                   "Hipergeométrica" = dhyper(valor, input$m, input$n_hip, input$k),
                   "t-Student" = dt(valor, input$df_t),
                   "Chi-cuadrado" = dchisq(valor, input$df_chi),
                   "F" = df(valor, input$df1, input$df2),
                   "Exponencial" = dexp(valor, input$rate)
    )

    list(
      texto = paste0("P(X = ", valor, ") = ", round(prob, 4)),
      valor = valor,
      prob = prob,
      tipo = "puntual"
    )
  }

  calcular_acumulada <- function() {
    valor <- input$valor_x
    prob <- switch(input$distribucion,
                   "Binomial" = pbinom(valor, input$n, input$p),
                   "Poisson" = ppois(valor, input$lambda),
                   "Normal" = pnorm(valor, input$media, input$sd),
                   "Uniforme Discreta" = punif(valor, input$min_unif, input$max_unif),
                   "Binomial Negativa" = pnbinom(valor - input$r, input$r, input$p_neg),
                   "Hipergeométrica" = phyper(valor, input$m, input$n_hip, input$k),
                   "t-Student" = pt(valor, input$df_t),
                   "Chi-cuadrado" = pchisq(valor, input$df_chi),
                   "F" = pf(valor, input$df1, input$df2),
                   "Exponencial" = pexp(valor, input$rate)
    )

    list(
      texto = paste0("P(X ≤ ", valor, ") = ", round(prob, 4)),
      valor = valor,
      prob = prob,
      tipo = "acumulada"
    )
  }

  calcular_mayor <- function() {
    valor <- input$valor_x
    prob <- switch(input$distribucion,
                   "Binomial" = 1 - pbinom(valor, input$n, input$p),
                   "Poisson" = 1 - ppois(valor, input$lambda),
                   "Normal" = 1 - pnorm(valor, input$media, input$sd),
                   "Uniforme Discreta" = 1 - punif(valor, input$min_unif, input$max_unif),
                   "Binomial Negativa" = 1 - pnbinom(valor - input$r, input$r, input$p_neg),
                   "Hipergeométrica" = 1 - phyper(valor, input$m, input$n_hip, input$k),
                   "t-Student" = 1 - pt(valor, input$df_t),
                   "Chi-cuadrado" = 1 - pchisq(valor, input$df_chi),
                   "F" = 1 - pf(valor, input$df1, input$df2),
                   "Exponencial" = 1 - pexp(valor, input$rate)
    )

    list(
      texto = paste0("P(X > ", valor, ") = ", round(prob, 4)),
      valor = valor,
      prob = prob,
      tipo = "mayor"
    )
  }

  calcular_cuantil <- function() {
    p <- input$prob_p
    cuantil <- switch(input$distribucion,
                      "Binomial" = qbinom(p, input$n, input$p),
                      "Poisson" = qpois(p, input$lambda),
                      "Normal" = qnorm(p, input$media, input$sd),
                      "Uniforme Discreta" = qunif(p, input$min_unif, input$max_unif),
                      "Binomial Negativa" = qnbinom(p, input$r, input$p_neg) + input$r,
                      "Hipergeométrica" = qhyper(p, input$m, input$n_hip, input$k),
                      "t-Student" = qt(p, input$df_t),
                      "Chi-cuadrado" = qchisq(p, input$df_chi),
                      "F" = qf(p, input$df1, input$df2),
                      "Exponencial" = qexp(p, input$rate)
    )

    list(
      texto = paste0("El cuantil q tal que P(X ≤ q) = ", p, " es: ", round(cuantil, 4)),
      valor = cuantil,
      prob = p,
      tipo = "cuantil"
    )
  }

  # Función de graficado -----
  graficar_distribucion <- function(resultado) {
    dist <- input$distribucion
    tipo <- input$tipo_calculo
    es_discreta <- input$tipo_dist == "Discreta"

    # --- Validaciones seguras ---
    if (dist == "Uniforme Discreta") {
      req(!is.null(input$min_unif), !is.null(input$max_unif))
      validate(need(input$min_unif < input$max_unif, "El máximo debe ser mayor que el mínimo."))
    }
    if (dist == "Binomial") req(input$n, input$p)
    if (dist == "Poisson") req(input$lambda)
    if (dist == "Normal") req(input$media, input$sd)
    if (dist == "Exponencial") req(input$rate)

    # --- Rango de valores ---
    rango <- switch(dist,
                    "Binomial" = 0:input$n,
                    "Poisson" = 0:max(20, 3 * input$lambda),
                    "Normal" = seq(input$media - 4 * input$sd, input$media + 4 * input$sd, length.out = 400),
                    "Uniforme Discreta" = seq(input$min_unif, input$max_unif, by = 1),
                    "Binomial Negativa" = input$r:(input$r + 5 * input$r),
                    "t-Student" = seq(-4, 4, length.out = 400),
                    "Chi-cuadrado" = seq(0, qchisq(0.999, input$df_chi), length.out = 400),
                    "F" = seq(0, qf(0.999, input$df1, input$df2), length.out = 400),
                    "Exponencial" = seq(0, qexp(0.999, input$rate), length.out = 400)
    )

    # --- Cálculo de densidad o probabilidad ---
    datos <- if (es_discreta) {
      data.frame(
        x = rango,
        y = switch(dist,
                   "Binomial" = dbinom(rango, input$n, input$p),
                   "Poisson" = dpois(rango, input$lambda),
                   "Uniforme Discreta" = rep(1 / (input$max_unif - input$min_unif + 1), length(rango)),
                   "Binomial Negativa" = dnbinom(rango - input$r, input$r, input$p_neg)
        )
      )
    } else {
      data.frame(
        x = rango,
        y = switch(dist,
                   "Normal" = dnorm(rango, input$media, input$sd),
                   "t-Student" = dt(rango, input$df_t),
                   "Chi-cuadrado" = dchisq(rango, input$df_chi),
                   "F" = df(rango, input$df1, input$df2),
                   "Exponencial" = dexp(rango, input$rate)
        )
      )
    }

    # --- Valor actual (x o q) ---
    valor <- if (tipo == "cuantil") resultado$valor else input$valor_x
    req(!is.null(valor))

    # --- Gráfico base ---
    p <- ggplot(datos, aes(x = x, y = y)) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Distribución", dist),
           x = "Valor",
           y = ifelse(es_discreta, "Probabilidad", "Densidad"))

    # --- Sombreado según tipo ---
    if (tipo %in% c("acumulada", "mayor")) {
      if (es_discreta) {
        if (tipo == "acumulada") {
          datos_sombra <- subset(datos, x <= valor)
        } else {
          datos_sombra <- subset(datos, x > valor)
        }
        p <- p +
          geom_bar(data = datos, stat = "identity", fill = "grey80") +
          geom_bar(data = datos_sombra, stat = "identity", fill = "skyblue") +
          geom_point(size = 3, color = "navy")
      } else {
        if (tipo == "acumulada") {
          datos_sombra <- subset(datos, x <= valor)
        } else {
          datos_sombra <- subset(datos, x > valor)
        }
        p <- p +
          geom_area(data = datos_sombra, aes(x, y), fill = "skyblue", alpha = 0.5) +
          geom_line(color = "navy", size = 1)
      }
    } else if (tipo == "puntual") {
      if (es_discreta) {
        p <- p +
          geom_bar(stat = "identity", fill = "grey80") +
          geom_bar(data = subset(datos, x == valor), stat = "identity", fill = "tomato") +
          geom_point(size = 3, color = "navy")
      } else {
        p <- p +
          geom_line(color = "navy", size = 1) +
          geom_vline(xintercept = valor, color = "tomato", linetype = "dashed", size = 1)
      }
    } else if (tipo == "cuantil") {
      p <- p +
        geom_line(color = "navy", size = 1) +
        geom_vline(xintercept = valor, color = "tomato", linetype = "dashed", size = 1)
    }

    p
  }

  # Variables reactivas para controlar la visualización
  mostrar_ejercicios <- reactiveVal(0)

  observeEvent(input$mostrar_ejercicios, {
    mostrar_ejercicios(1)
    generar_ejercicio()
  })

  observeEvent(input$volver_graficos, {
    mostrar_ejercicios(0)
  })

  # Variables reactivas para los ejercicios
  # Variables reactivas para los ejercicios
  ejercicio_actual <- reactiveValues(
    enunciado = NULL,
    distribucion = NULL,
    parametros = NULL,
    tipo = NULL,
    valor = NULL,
    respuesta_correcta = NULL
  )

  # Función para generar un nuevo ejercicio
  generar_ejercicio <- function() {
    # Seleccionar una distribución aleatoria
    dists <- c("Binomial", "Poisson", "Normal", "Uniforme Discreta",
               "Exponencial", "Binomial Negativa")

    ejercicio_actual$distribucion <- sample(dists, 1)

    # Generar parámetros aleatorios según la distribución
    ejercicio_actual$parametros <- switch(
      ejercicio_actual$distribucion,
      "Binomial" = {
        list(
          n = sample(5:20, 1),
          p = round(runif(1, 0.1, 0.9), 2)
        )
      },
      "Poisson" = {
        list(
          lambda = round(runif(1, 1, 10), 1)
        )
      },
      "Normal" = {
        list(
          media = round(rnorm(1, 0, 5)),
          sd = round(runif(1, 1, 5), 1)
        )
      },
      "Uniforme Discreta" = {
        list(
          min = 0,
          max = sample(5:20, 1))
      },
      "Exponencial" = {
        list(
          rate = round(runif(1, 0.1, 2), 1))
      },
      "Binomial Negativa" = {
        list(
          r = sample(2:5, 1),
          p = round(runif(1, 0.2, 0.8), 1))
      }
    )

    # Seleccionar un tipo de pregunta aleatorio
    tipos <- c("puntual", "acumulada", "mayor")
    ejercicio_actual$tipo <- sample(tipos, 1)

    # Generar un valor x apropiado
    ejercicio_actual$valor <- switch(
      ejercicio_actual$distribucion,
      "Binomial" = sample(0:ejercicio_actual$parametros$n, 1),
      "Poisson" = sample(0:15, 1),
      "Normal" = round(rnorm(1, ejercicio_actual$parametros$media,
                             ejercicio_actual$parametros$sd), 1),
      "Uniforme Discreta" = sample(ejercicio_actual$parametros$min:ejercicio_actual$parametros$max, 1),
      "Exponencial" = round(rexp(1, ejercicio_actual$parametros$rate), 1),
      "Binomial Negativa" = sample(ejercicio_actual$parametros$r:(ejercicio_actual$parametros$r+10), 1)
    )

    # Si la distribución es continua y el tipo es puntual, forzamos la probabilidad a cero
    distribuciones_continuas <- c("Normal", "Exponencial")
    if (ejercicio_actual$distribucion %in% distribuciones_continuas &&
        ejercicio_actual$tipo == "puntual") {

      ejercicio_actual$respuesta_correcta <- 0
    } else {
      # Calcular la respuesta correcta normalmente
      ejercicio_actual$respuesta_correcta <- calcular_respuesta(
        ejercicio_actual$distribucion,
        ejercicio_actual$parametros,
        ejercicio_actual$tipo,
        ejercicio_actual$valor
      )
    }

    # Generar el enunciado
    ejercicio_actual$enunciado <- generar_enunciado(
      ejercicio_actual$distribucion,
      ejercicio_actual$parametros,
      ejercicio_actual$tipo,
      ejercicio_actual$valor
    )
  }

  # Función para calcular la respuesta correcta
  calcular_respuesta <- function(dist, params, tipo, valor) {
    switch(tipo,
           "puntual" = switch(dist,
                              "Binomial" = dbinom(valor, params$n, params$p),
                              "Poisson" = dpois(valor, params$lambda),
                              "Normal" = 0,  # Probabilidad puntual continua = 0
                              "Uniforme Discreta" = 1/(params$max - params$min + 1),
                              "Exponencial" = 0,  # Probabilidad puntual continua = 0
                              "Binomial Negativa" = dnbinom(valor - params$r, params$r, params$p)),
           "acumulada" = switch(dist,
                                "Binomial" = pbinom(valor, params$n, params$p),
                                "Poisson" = ppois(valor, params$lambda),
                                "Normal" = pnorm(valor, params$media, params$sd),
                                "Uniforme Discreta" = punif(valor, params$min, params$max),
                                "Exponencial" = pexp(valor, params$rate),
                                "Binomial Negativa" = pnbinom(valor - params$r, params$r, params$p)),
           "mayor" = switch(dist,
                            "Binomial" = 1 - pbinom(valor, params$n, params$p),
                            "Poisson" = 1 - ppois(valor, params$lambda),
                            "Normal" = 1 - pnorm(valor, params$media, params$sd),
                            "Uniforme Discreta" = 1 - punif(valor, params$min, params$max),
                            "Exponencial" = 1 - pexp(valor, params$rate),
                            "Binomial Negativa" = 1 - pnbinom(valor - params$r, params$r, params$p))
    )
  }

  # Función para generar el enunciado del ejercicio (permite mensaje adicional opcional)
  generar_enunciado <- function(dist, params, tipo, valor, mensaje_extra = NULL) {
    texto_dist <- switch(dist,
                         "Binomial" = paste0("Binomial(n=", params$n, ", p=", params$p, ")"),
                         "Poisson" = paste0("Poisson(λ=", params$lambda, ")"),
                         "Normal" = paste0("Normal(μ=", params$media, ", σ=", params$sd, ")"),
                         "Uniforme Discreta" = paste0("Uniforme Discreta(a=", params$min, ", b=", params$max, ")"),
                         "Exponencial" = paste0("Exponencial(λ=", params$rate, ")"),
                         "Binomial Negativa" = paste0("Binomial Negativa(r=", params$r, ", p=", params$p, ")"))

    pregunta <- switch(tipo,
                       "puntual" = paste0("P(X = ", valor, ")"),
                       "acumulada" = paste0("P(X ≤ ", valor, ")"),
                       "mayor" = paste0("P(X > ", valor, ")"))

    extra <- if (!is.null(mensaje_extra)) {
      paste0("<p style='color: red; font-style: italic;'>", mensaje_extra, "</p>")
    } else {
      ""
    }

    HTML(paste0(
      "<h4>Distribución ", texto_dist, "</h4>",
      "<p>Calcula ", pregunta, "</p>",
      "<p>Redondea tu respuesta a 4 decimales</p>",
      extra
    ))
  }

  # Mostrar el enunciado del ejercicio
  output$enunciado_ejercicio <- renderUI({
    if (is.null(ejercicio_actual$enunciado)) {
      HTML("<p>Presiona 'Verificar Respuesta' para comenzar</p>")
    } else {
      ejercicio_actual$enunciado
    }
  })

  # Verificar la respuesta del usuario
  observeEvent(input$verificar_respuesta, {
    req(ejercicio_actual$respuesta_correcta)

    respuesta_usuario <- input$respuesta_usuario
    respuesta_correcta <- ejercicio_actual$respuesta_correcta

    if (is.na(respuesta_usuario)) {
      output$retroalimentacion <- renderUI({
        HTML("<div style='color: red;'>Por favor ingresa una respuesta</div>")
      })
    } else {
      # Comparar respuestas con cierta tolerancia para decimales
      diferencia <- abs(respuesta_usuario - respuesta_correcta)

      if (diferencia < 0.001) {
        mensaje <- paste0(
          "<div style='color: green; font-weight: bold;'>¡Correcto! ",
          "La respuesta es ", round(respuesta_correcta, 4), "</div>"
        )
        # Generar nuevo ejercicio después de 2 segundos si la respuesta es correcta
        delay(2000, generar_ejercicio())
      } else {
        mensaje <- paste0(
          "<div style='color: red;'>Incorrecto. ",
          "La respuesta correcta es ", round(respuesta_correcta, 4), "</div>"
        )
      }

      # Añadir mensaje explicativo si es una distribución continua puntual
      if (ejercicio_actual$tipo == "puntual" &&
          ejercicio_actual$distribucion %in% c("Normal", "Exponencial")) {
        mensaje <- paste0(
          mensaje,
          "<br><br><div style='color: orange; font-style: italic;'>
        En una distribución continua, la probabilidad en un punto es siempre cero.
        </div>"
        )
      }

      output$retroalimentacion <- renderUI({
        HTML(mensaje)
      })
    }
  })


  # Gráfico para el ejercicio
  output$grafico_ejercicio <- renderPlot({
    req(ejercicio_actual$distribucion)

    dist <- ejercicio_actual$distribucion
    params <- ejercicio_actual$parametros
    valor <- ejercicio_actual$valor
    tipo <- ejercicio_actual$tipo

    # Generar datos para el gráfico
    datos <- switch(dist,
                    "Binomial" = {
                      x <- 0:params$n
                      y <- dbinom(x, params$n, params$p)
                      data.frame(x = x, y = y)
                    },
                    "Poisson" = {
                      x <- 0:max(15, 3*params$lambda)
                      y <- dpois(x, params$lambda)
                      data.frame(x = x, y = y)
                    },
                    "Normal" = {
                      x <- seq(params$media - 4*params$sd, params$media + 4*params$sd, length.out = 200)
                      y <- dnorm(x, params$media, params$sd)
                      data.frame(x = x, y = y)
                    },
                    "Uniforme Discreta" = {
                      x <- params$min:params$max
                      y <- rep(1/(params$max - params$min + 1), length(x))
                      data.frame(x = x, y = y)
                    },
                    "Exponencial" = {
                      x <- seq(0, qexp(0.999, params$rate), length.out = 200)
                      y <- dexp(x, params$rate)
                      data.frame(x = x, y = y)
                    },
                    "Binomial Negativa" = {
                      x <- params$r:(params$r + 20)
                      y <- dnbinom(x - params$r, params$r, params$p)
                      data.frame(x = x, y = y)
                    })

    # Crear gráfico base
    p <- ggplot(datos, aes(x = x, y = y)) +
      theme_minimal() +
      labs(title = paste("Distribución", dist),
           x = "Valor", y = ifelse(dist %in% c("Binomial", "Poisson", "Uniforme Discreta", "Binomial Negativa"),
                                   "Probabilidad", "Densidad"))

    # Añadir geometría según sea discreta o continua
    if (dist %in% c("Binomial", "Poisson", "Uniforme Discreta", "Binomial Negativa")) {
      p <- p +
        geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
        geom_point(size = 3, color = "navy")

      # Añadir polígono de frecuencias
      datos_poligono <- datos
      primer_punto <- data.frame(x = min(datos_poligono$x) - 1, y = 0)
      ultimo_punto <- data.frame(x = max(datos_poligono$x) + 1, y = 0)
      datos_poligono <- rbind(primer_punto, datos_poligono, ultimo_punto)

      p <- p + geom_line(data = datos_poligono, aes(group = 1),
                         color = "darkblue", size = 1, linetype = "solid")
    } else {
      p <- p + geom_line(color = "navy", size = 1)
    }

    # Resaltar área según el tipo de pregunta
    if (tipo == "puntual") {
      if (dist %in% c("Binomial", "Poisson", "Uniforme Discreta", "Binomial Negativa")) {
        p <- p + geom_bar(data = datos %>% filter(x == valor),
                          stat = "identity", fill = "red", width = 0.7)
      } else {
        p <- p +
          geom_vline(xintercept = valor, color = "red") +
          annotate("point", x = valor, y = calcular_respuesta(dist, params, tipo, valor),
                   color = "red", size = 3)
      }
    } else if (tipo == "acumulada") {
      if (dist %in% c("Binomial", "Poisson", "Uniforme Discreta", "Binomial Negativa")) {
        datos_area <- datos %>% filter(x <= valor)
        if (nrow(datos_area) > 0) {
          datos_area_pol <- rbind(
            data.frame(x = min(datos_area$x) - 1, y = 0),
            datos_area,
            data.frame(x = max(datos_area$x), y = 0)
          )
          p <- p +
            geom_polygon(data = datos_area_pol, aes(x = x, y = y),
                         fill = "red", alpha = 0.3)
        }
      } else {
        datos_area <- datos %>% filter(x <= valor)
        p <- p + geom_area(data = datos_area, fill = "red", alpha = 0.3)
      }
      p <- p + geom_vline(xintercept = valor, linetype = "dashed", color = "red", size = 1)
    } else if (tipo == "mayor") {
      if (dist %in% c("Binomial", "Poisson", "Uniforme Discreta", "Binomial Negativa")) {
        datos_area <- datos %>% filter(x >= valor)
        if (nrow(datos_area) > 0) {
          datos_area_pol <- rbind(
            data.frame(x = min(datos_area$x), y = 0),
            datos_area,
            data.frame(x = max(datos_area$x) + 1, y = 0)
          )
          p <- p +
            geom_polygon(data = datos_area_pol, aes(x = x, y = y),
                         fill = "blue", alpha = 0.3)
        }
      } else {
        datos_area <- datos %>% filter(x >= valor)
        p <- p + geom_area(data = datos_area, fill = "blue", alpha = 0.3)
      }
      p <- p + geom_vline(xintercept = valor, linetype = "dashed", color = "blue", size = 1)
    }

    p
  })
}

shinyApp(ui, server)
