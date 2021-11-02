#' @title Distribuciones de probabilidad.
#'
#' @description Aplicación interactiva donde se representa las principales distribuciones de probabilidad unidimensionales: Binomial, Poisson, Uniforme, Exponencial y Normal.
#' @usage distribuciones.probabilidad()
#'
#' @return No devuelve un valor, Es una aplicación shiny.
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
