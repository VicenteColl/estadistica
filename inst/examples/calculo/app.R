library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  # Logo y botón de cierre en la parte superior derecha
  div(style = "position: absolute; right: 20px; top: 10px; z-index: 1000;",
      a(href = 'https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ',
        target = '_blank',
        img(src = 'e_R_logoB_web.jpg',
            title = 'Canal youtube:\nLa magia de estadistica',
            height = "30px",
            style = "margin-right: 15px;")),
      a(href = "javascript:window.close()",
        title = "Cerrar",
        icon("power-off"))
  ),

  titlePanel("Aprendizaje de Distribuciones de Probabilidad"),
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
          radioButtons("tipo_calculo", "Tipo de cálculo:",
                       choices = c("Probabilidad puntual (P(X = x))" = "puntual",
                                   "Probabilidad acumulada (P(X ≤ x))" = "acumulada",
                                   "Probabilidad mayor que (P(X > x))" = "mayor",
                                   "Cuantil (P(X ≤ q) = p)" = "cuantil")),
          conditionalPanel(
            condition = "input.tipo_calculo != 'cuantil'",
            numericInput("valor_x", "Valor de x:", value = 0)
          ),
          conditionalPanel(
            condition = "input.tipo_calculo == 'cuantil'",
            sliderInput("prob_p", "Probabilidad p:", min = 0, max = 1, value = 0.5, step = 0.01),
            numericInput("prob_p_num", "O ingrese p numéricamente:", min = 0, max = 1, value = 0.5, step = 0.01)
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
        div(style = "width: 200px; margin-bottom: 15px;",  # Control del ancho del input
            numericInput("respuesta_usuario", "Tu respuesta:",
                         value = NA, min = 0, max = 1, step = 0.001,
                         width = "100%")),
        # Convertir comas a puntos en el input
        tags$script(HTML(
          "$(document).on('keyup', '#respuesta_usuario', function() {
            var value = $(this).val().replace(',', '.');
            if(value !== $(this).val()) {
              $(this).val(value);
            }
          });"
        )),
        # Botones superiores (Comprobar y Ayuda)
        fluidRow(
          column(6, actionButton("verificar_respuesta", "Comprobar respuesta",
                                 style = "width: 100%; margin-bottom: 15px;")),
          column(6, actionButton("ayuda_grafica", "Ayuda gráfica",
                                 style = "width: 100%; background-color: #d4edda; margin-bottom: 15px;"))
        ),
        htmlOutput("retroalimentacion"),
        # Solo mostrar gráfico cuando el contador es impar
        conditionalPanel(
          condition = "output.mostrar_grafico_condicional == true",
          plotOutput("grafico_ejercicio", height = "300px")
        ),
        # Botones inferiores (Atrás y Nuevo ejercicio)
        fluidRow(
          # Primera columna con los botones
          column(6,
                 fluidRow(
                   column(6, actionButton("volver_graficos", "ATRÁS",
                                          style = "width: 100%; background-color: #f8f9fa; border: 1px solid #ddd;")),
                   column(6, actionButton("nuevo_ejercicio", "NUEVO EJERCICIO",
                                          style = "width: 100%; background-color: #4CAF50; color: white;"))
                 ),
                 # Segunda columna vacía
                 column(6, div())
          )
        )
      )
    }
  })

  # Output condicional para mostrar el gráfico
  output$mostrar_grafico_condicional <- reactive({
    contador_ayuda() %% 2 == 1
  })
  outputOptions(output, "mostrar_grafico_condicional", suspendWhenHidden = FALSE)

  # Mostrar gráfico por defecto al iniciar
  output$grafico <- renderPlot({
    req(input$distribucion)

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

  observeEvent(input$mostrar_ejercicios, {
    mostrar_ejercicios(1)
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})  # Limpiar retroalimentación
    generar_ejercicio()
  })

  observeEvent(input$volver_graficos, {
    mostrar_ejercicios(0)
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})
  })

  observeEvent(input$nuevo_ejercicio, {
    contador_ayuda(0)
    output$retroalimentacion <- renderUI({NULL})  # Limpiar retroalimentación al nuevo ejercicio
    updateNumericInput(session, "respuesta_usuario", value = NA)  # Limpiar el input
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
                 numericInput("p", "Probabilidad de éxito (p):", value = 0.5, min = 0, max = 1)
               )
             },
             "Poisson" = {
               numericInput("lambda", "Tasa (λ):", value = 1, min = 0)
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
    es_discreta <- input$tipo_dist == "Discreta"

    # Rango de valores para graficar
    rango <- switch(dist,
                    "Binomial" = 0:input$n,
                    "Poisson" = 0:max(20, 3*input$lambda),
                    "Normal" = seq(input$media - 4*input$sd, input$media + 4*input$sd, length.out = 200),
                    "Uniforme Discreta" = input$min_unif:input$max_unif,
                    "Binomial Negativa" = input$r:(input$r + 5*input$r),
                    "Hipergeométrica" = max(0, input$k - input$n_hip):min(input$k, input$m),
                    "t-Student" = seq(-4, 4, length.out = 200),
                    "Chi-cuadrado" = seq(0, qchisq(0.999, input$df_chi), length.out = 200),
                    "F" = seq(0, qf(0.999, input$df1, input$df2), length.out = 200),
                    "Exponencial" = seq(0, qexp(0.999, input$rate), length.out = 200)
    )

    # Datos para el gráfico
    datos <- if (es_discreta) {
      data.frame(
        x = rango,
        y = switch(dist,
                   "Binomial" = dbinom(rango, input$n, input$p),
                   "Poisson" = dpois(rango, input$lambda),
                   "Uniforme Discreta" = dunif(rango, input$min_unif, input$max_unif),
                   "Binomial Negativa" = dnbinom(rango - input$r, input$r, input$p_neg),
                   "Hipergeométrica" = dhyper(rango, input$m, input$n_hip, input$k)
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

    # Crear gráfico base
    p <- ggplot(datos, aes(x = x, y = y)) +
      theme_minimal() +
      labs(title = paste("Distribución", dist),
           x = "Valor", y = ifelse(es_discreta, "Probabilidad", "Densidad"))

    # Añadir geometría según sea discreta o continua
    if (es_discreta) {
      p <- p +
        geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
        geom_point(size = 3, color = "navy")

      # Añadir polígono de frecuencias que corte los ejes
      if (input$mostrar_area && input$tipo_calculo != "puntual") {
        # Extender los datos para que el polígono toque los ejes
        datos_poligono <- datos
        if (nrow(datos_poligono) > 0) {
          # Añadir punto antes del primer valor (x = min - 1, y = 0)
          primer_punto <- data.frame(x = min(datos_poligono$x) - 1, y = 0)
          # Añadir punto después del último valor (x = max + 1, y = 0)
          ultimo_punto <- data.frame(x = max(datos_poligono$x) + 1, y = 0)
          datos_poligono <- rbind(primer_punto, datos_poligono, ultimo_punto)
        }

        p <- p + geom_line(data = datos_poligono, aes(group = 1),
                           color = "darkblue", size = 1, linetype = "solid")
      }
    } else {
      p <- p + geom_line(color = "navy", size = 1)
    }

    # Resaltar área según el tipo de cálculo
    if (input$mostrar_area && input$tipo_calculo != "puntual") {
      if (es_discreta) {
        # Para distribuciones discretas con polígono de frecuencias
        if (input$tipo_calculo == "acumulada") {
          datos_area <- datos %>% filter(x <= resultado$valor)
          if (nrow(datos_area) > 0) {
            # Crear datos para el área del polígono
            datos_area_pol <- rbind(
              data.frame(x = min(datos_area$x) - 1, y = 0),
              datos_area,
              data.frame(x = max(datos_area$x), y = 0)
            )

            p <- p +
              geom_polygon(data = datos_area_pol, aes(x = x, y = y),
                           fill = "red", alpha = 0.3)
          }
        } else if (input$tipo_calculo == "mayor") {
          datos_area <- datos %>% filter(x >= resultado$valor)
          if (nrow(datos_area) > 0) {
            # Crear datos para el área del polígono
            datos_area_pol <- rbind(
              data.frame(x = min(datos_area$x), y = 0),
              datos_area,
              data.frame(x = max(datos_area$x) + 1, y = 0)
            )

            p <- p +
              geom_polygon(data = datos_area_pol, aes(x = x, y = y),
                           fill = "blue", alpha = 0.3)
          }
        } else if (input$tipo_calculo == "cuantil") {
          datos_area <- datos %>% filter(x <= resultado$valor)
          if (nrow(datos_area) > 0) {
            # Crear datos para el área del polígono
            datos_area_pol <- rbind(
              data.frame(x = min(datos_area$x) - 1, y = 0),
              datos_area,
              data.frame(x = max(datos_area$x), y = 0)
            )

            p <- p +
              geom_polygon(data = datos_area_pol, aes(x = x, y = y),
                           fill = "green", alpha = 0.3)
          }
        }

        # Añadir línea vertical
        p <- p + geom_vline(xintercept = resultado$valor,
                            linetype = "dashed",
                            color = switch(input$tipo_calculo,
                                           "acumulada" = "red",
                                           "mayor" = "blue",
                                           "cuantil" = "green"),
                            size = 1)
      } else {
        # Para distribuciones continuas (mismo método anterior)
        if (input$tipo_calculo == "acumulada") {
          datos_area <- datos %>% filter(x <= resultado$valor)
          p <- p +
            geom_area(data = datos_area, fill = "red", alpha = 0.3) +
            geom_vline(xintercept = resultado$valor, linetype = "dashed", color = "red")
        } else if (input$tipo_calculo == "mayor") {
          datos_area <- datos %>% filter(x >= resultado$valor)
          p <- p +
            geom_area(data = datos_area, fill = "blue", alpha = 0.3) +
            geom_vline(xintercept = resultado$valor, linetype = "dashed", color = "blue")
        } else if (input$tipo_calculo == "cuantil") {
          datos_area <- datos %>% filter(x <= resultado$valor)
          p <- p +
            geom_area(data = datos_area, fill = "green", alpha = 0.3) +
            geom_vline(xintercept = resultado$valor, linetype = "dashed", color = "green")
        }
      }
    } else if (input$tipo_calculo == "puntual") {
      if (es_discreta) {
        p <- p +
          geom_bar(data = datos %>% filter(x == resultado$valor),
                   stat = "identity", fill = "red", width = 0.7)
      } else {
        p <- p +
          geom_vline(xintercept = resultado$valor, color = "red") +
          annotate("point", x = resultado$valor, y = resultado$prob,
                   color = "red", size = 3)
      }
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

        # Calcular la respuesta correcta
        ejercicio_actual$respuesta_correcta <- calcular_respuesta(
          ejercicio_actual$distribucion,
          ejercicio_actual$parametros,
          ejercicio_actual$tipo,
          ejercicio_actual$valor
        )

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
                              "Normal" = dnorm(valor, params$media, params$sd),
                              "Uniforme Discreta" = 1/(params$max - params$min + 1),
                              "Exponencial" = dexp(valor, params$rate),
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

  # Función para generar el enunciado del ejercicio
  generar_enunciado <- function(dist, params, tipo, valor) {
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

    HTML(paste0(
      "<h4>Distribución ", texto_dist, "</h4>",
      "<p>Calcula ", pregunta, "</p>",
      "<p>Redondea tu respuesta a 4 decimales</p>"
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
