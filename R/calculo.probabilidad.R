#' @title Cálculo de probabilidades.
#'
#' @description Aplicación interactiva para practicar el cálculo de probabilidades a partir de distintas distribuciones de probabilidad.
#' @usage calculo.probabilidad()
#'
#' @return No devuelve un valor, es una aplicación shiny.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @import shiny
#'
#' @export
calculo.probabilidad <- function() {
  appDir <- system.file("examples/calculo", package = "estadistica")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
