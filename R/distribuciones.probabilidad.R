#' @title Distribuciones de probabilidad.
#'
#' @description Aplicación interactiva donde se representa las principales distribuciones de probabilidad unidimensionales: Binomial, Poisson, Uniforme, Exponencial y Normal.
#' @usage distribuciones.probabilidad()
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
#' @import shiny shinydashboard
#'
#' @export
distribuciones.probabilidad <- function() {

  appDir <- system.file("examples/probabilidad", package = "estadistica")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
