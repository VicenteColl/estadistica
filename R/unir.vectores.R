#' @title Unir vectores.
#'
#' @description Une dos o más vectores numéricos de igual o distinta longitud.
#'
#' \if{html}{\figure{qrleerdatos.png}{options: style="width: 25\%;" alt="Figure: qrleerdatos.png"}}
#' \if{latex}{\figure{qrleerdatos.png}{options: width=3cm}}
#'
#' @usage unir.vectores(...)
#'
#' @param ... Introducir los nombres de los objetos, vectores, que se quiere unir. Si los vectores tienen distinta longitud se rellenarán los espacios con NAs.
#'
#' @return La función devuelve un dataframe.
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
#' @export
unir.vectores <- function(...){

  names <- as.character(substitute(list(...)))[-1L]

  if (length(names)<2)
    warning("No hay vectores que unir")

  lista <- list(...)
  max.length <- max(sapply(lista, length))

  lista <- lapply(lista, function(x) { c(x, rep(NA, max.length-length(x)))})

  df <- do.call(cbind, lista)
  df <- as.data.frame(df)
  names(df) <- names

  return(df)
}

