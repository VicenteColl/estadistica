#' @title Convergencia de la varianza y cuasivarianza muestral.
#'
#' @description Gráfico dinámico que ilustra la convergencia de la varianza y cuasi-varianza muestral a medida que aumenta el tamaño muestral.
#' @usage convergencia.varianza()
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#'
#' @import dplyr ggplot2
#'
#' @export
convergencia.varianza <- function() {

  cat("Al tratarse de un gr\u00e1fico din\u00e1mico llevar\u00e1 unos segundos generarlo.\n\n")
  cat("Para visualizarlo bien haz clic sobre el Zoom")
  sumatorio <- 1500

  df <- data.frame(id = 1:100, n = sumatorio/(2:101),n_1 = sumatorio/(1:100))

  df2 <- data.frame(n = rep(df$id,2),
                    varianza = c(df$n,df$n_1),
                    grupo = rep(c("varianza.muestral","cuasi_varianza.muestral"),each=nrow(df)),
                    grupo2 = rep(c("var","cuasivar"),each=nrow(df)))

  p <- ggplot(df2, aes(x=n, y=varianza, group=grupo, frame = n, color = grupo)) +
    geom_line() +
    geom_segment(aes(xend=max(n), yend = varianza), linetype=2, color="darkgreen") +
    geom_point(size = 2) +
    geom_text(aes(x = max(n)+.1, label = paste(sprintf("%5.2f", varianza), "\n ", grupo2 ,sep="")), size = 3, hjust=0) +
    scale_color_manual(values = c("blue","orange4")) +
    labs(title = 'Convergencia de varianza y cuasivarianza muestral', y = 'Tipo varianza', x="Tama\u00f1o de la muestra") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = margin(5.5, 40, 5.5, 5.5))

  plot <- plotly::ggplotly(p)

  plot <- plot %>%
    plotly::layout(showlegend = FALSE) %>%
    plotly::animation_opts(1000, easing = "elastic", redraw = FALSE)

  return(plot)

}
