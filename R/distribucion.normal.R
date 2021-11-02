#' @title Distribución normal.
#'
#' @description Aplicación interactiva para comparar dos distribuciones normales.
#' @usage distribucion.normal()
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
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @import shiny
#'
#' @export
distribucion.normal <- function() {
  appDir <- system.file("examples/normal", package = "estadistica")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
