#' @title Convergencia de la varianza y cuasivarianza muestral.
#'
#' @description Gráfico dinámico que ilustra la convergencia de la varianza y cuasi-varianza muestral a medida que aumenta el tamaño muestral.
#' @usage convergencia.varianza()
#'
#' @return Devuelve un gráfico que es un objeto de la clase \code{plotly} y \code{htmlwidget}.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @import dplyr ggplot2 plotly
#' @export
convergencia.varianza <- function(){
  
  cat("Generando animación... Cuando se cargue haz clic en el botón: Play\n")
  cat(
    "\033[1mConvergencia:\033[0m Observa como la diferencia entre varianza muestral y cuasivarianza muestral disminuye a medida que aumenta el tamaño de la muestra.\n"
  )
  cat(
    "\033[1;31mCuánto mayor sea el tamaño de la muestra menor será la diferencia entre varianza y cuasivarianza muestral\033[0m\n"
  )

  
# generar datos aleatorios
  
  set.seed(123)
  
  n_obs <- 150
  
  # velocidad del grafico
  velocidad <- 800
  
  media <- 500
  sd <- 12
  
  x <- rnorm(
    n = n_obs,
    mean = media,
    sd = sd
  )
  
# calculo de var y cuasivar
  df <- data.frame(
    n = 2:n_obs,
    varianza_muestral = NA_real_,
    cuasivarianza = NA_real_
  )
  
  for(i in 2:n_obs){
    
    muestra <- x[1:i]
    
    media_muestral <- mean(muestra)
    
    suma_cuadrados <- sum(
      (muestra - media_muestral)^2
    )
    
    # Varianza muestral
    df$varianza_muestral[df$n == i] <-
      suma_cuadrados / i
    
    # Cuasivarianza
    df$cuasivarianza[df$n == i] <-
      suma_cuadrados / (i - 1)
    
  }
  
# convergencia: diferencia <= 1
  df$diferencia <- abs(
    df$varianza_muestral -
      df$cuasivarianza
  )
  
  df$convergencia <- df$diferencia <= 1
  
# colores dinamicos
  
  df$color_var <- ifelse(
    df$convergencia,
    "darkgreen",
    "#1565C0"
  )
  
  df$color_cua <- ifelse(
    df$convergencia,
    "darkgreen",
    "#E65100"
  )
  
# formato long de datos
  
  df_var <- data.frame(
    n = df$n,
    valor = df$varianza_muestral,
    grupo = "Varianza muestral",
    color = df$color_var
  )
  
  df_cua <- data.frame(
    n = df$n,
    valor = df$cuasivarianza,
    grupo = "Cuasivarianza",
    color = df$color_cua
  )
  
  df2 <- bind_rows(
    df_var,
    df_cua
  )
  

  datos_anim <- bind_rows(
    
    lapply(1:nrow(df), function(i){
      
      df2 %>%
        filter(n <= df$n[i]) %>%
        mutate(frame = df$n[i])
      
    })
    
  )
  
# calculo difernecia entre var y cuasivar
  
  df_dif <- data.frame(
    
    n = df$n,
    
    ymin = pmin(
      df$varianza_muestral,
      df$cuasivarianza
    ),
    
    ymax = pmax(
      df$varianza_muestral,
      df$cuasivarianza
    ),
    
    diferencia = df$diferencia,
    
    frame = df$n
  )
  
# posicion texto de la diferencia
  
  # Texto bastante separado para mejorar legibilidad
  df_dif$y_texto <- df_dif$ymin - 1.8
  
# plot
  
  p <- plot_ly()

# convergencia    
  p <- p %>%
    
    add_ribbons(
      
      data = df_dif,
      
      x = ~n,
      
      ymin = ~ymin,
      ymax = ~ymax,
      
      frame = ~frame,
      
      fillcolor = "rgba(50,180,50,0.18)",
      
      line = list(
        color = "transparent"
      ),
      
      hoverinfo = "none",
      
      showlegend = FALSE
    )
  
# dibujo de var
  
  p <- p %>%
    
    add_trace(
      
      data = datos_anim %>%
        filter(grupo == "Varianza muestral"),
      
      x = ~n,
      y = ~valor,
      
      frame = ~frame,
      
      type = "scatter",
      
      mode = "lines+markers",
      
      name = "Varianza muestral",
      
      # Línea suavizada
      line = list(
        color = "rgba(21,101,192,0.45)",
        width = 3
      ),
      
      marker = list(
        
        size = 11,
        
        symbol = "circle",
        
        color = datos_anim %>%
          filter(grupo == "Varianza muestral") %>%
          pull(color),
        
        line = list(
          color = "white",
          width = 2
        )
      ),
      
      hovertemplate =
        paste(
          "<b>Varianza muestral</b><br>",
          "n = %{x}<br>",
          "Valor = %{y:.4f}<extra></extra>"
        )
    )
  
# dibujo de cuasivar
  
  p <- p %>%
    
    add_trace(
      
      data = datos_anim %>%
        filter(grupo == "Cuasivarianza"),
      
      x = ~n,
      y = ~valor,
      
      frame = ~frame,
      
      type = "scatter",
      
      mode = "lines+markers",
      
      name = "Cuasivarianza",
      
      line = list(
        color = "rgba(230,81,0,0.45)",
        width = 3
      ),
      
      marker = list(
        
        size = 10,
        
        symbol = "circle",
        
        color = datos_anim %>%
          filter(grupo == "Cuasivarianza") %>%
          pull(color),
        
        line = list(
          color = "white",
          width = 2
        )
      ),
      
      hovertemplate =
        paste(
          "<b>Cuasivarianza</b><br>",
          "n = %{x}<br>",
          "Valor = %{y:.4f}<extra></extra>"
        )
    )
  

# linea de guia de la diferencia entre var y cuasivar
  p <- p %>%
    
    add_segments(
      
      data = df_dif,
      
      x = ~n,
      xend = ~n,
      
      y = ~y_texto + 0.25,
      
      yend = ~(ymin + ymax)/2,
      
      frame = ~frame,
      
      line = list(
        color = "rgba(0,100,0,0.45)",
        width = 1.5,
        dash = "dot"
      ),
      
      hoverinfo = "none",
      
      showlegend = FALSE
    )
  
# informacion de la diferencia entre var y cuasivar
  
  p <- p %>%
    
    add_trace(
      
      data = df_dif,
      
      x = ~n,
      
      y = ~y_texto,
      
      frame = ~frame,
      
      type = "scatter",
      
      mode = "markers+text",
      
      text = ~paste0(
        "Dif = ",
        round(diferencia, 4)
      ),
      
      textposition = "bottom center",
      
      textfont = list(
        size = 12,
        color = "darkgreen",
        family = "Arial Black"
      ),
      
      # Burbuja de fondo
      marker = list(
        
        size = 18,
        
        color = "rgba(255,255,255,0.78)",
        
        line = list(
          color = "darkgreen",
          width = 1
        )
      ),
      
      hoverinfo = "none",
      
      showlegend = FALSE
    )
  
# diferencia pequenya los puntos son verdes
  p <- p %>%
    
    layout(
      
      title = list(
        
        text = paste0(
          "Convergencia entre varianza muestral y cuasivarianza",
          "<br><sup>",
          "Los puntos se vuelven verdes cuando la diferencia ≤ 1",
          "</sup>"
        ),
        
        x = 0.5
      ),
      
      xaxis = list(
        
        title = "<b>Tamaño muestral (n)</b>",
        
        range = c(2, n_obs),
        
        gridcolor = "rgba(0,0,0,0.08)"
      ),
      
      yaxis = list(
        
        title = "<b>Valor</b>",
        
        gridcolor = "rgba(0,0,0,0.08)"
      ),
      
      template = "simple_white",
      
      hovermode = "x unified",
      
      legend = list(
        
        orientation = "h",
        
        x = 0.22,
        y = -0.15
      )
    )
  
# animacion
  
  p <- p %>%
    
    animation_opts(
      
      frame = velocidad,
      
      transition = 150,
      
      redraw = FALSE,
      
      easing = "linear"
    ) %>%
    
    animation_slider(
      
      currentvalue = list(
        prefix = "n = "
      )
    )
  
  return(p)
  
}
