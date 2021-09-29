#' @title Momento central.
#'
#' @description Calcula los momentos centrales respecto de la media.
#' @usage momento.central(x, orden)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param orden Es un valor numérico que representa el orden del momento central (orden = {1,2,3,4,...})
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
#' Universidad de Valencia (España)
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr
#'
#' @export
momento.central <- function(x, orden){

  orden <- as.integer(orden)

  if(!is.integer(orden)){

    stop("El orden del momento central debe ser un valor num\\u00e9rico entero")

  }

  x <- data.frame(x)
  varnames <- names(x)

  if(length(x) > 1){

    stop("Esta funci\\u00f3n solo funciona para una variable y parece que tus datos tienen varias variables")

  }

  clase <- sapply(x,class)

  if (!(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }


  momento <- x %>%
    mutate(media_x = media(x),
           momento = (x-media_x)^orden) %>%
    summarize(momento = sum(momento)/n()) %>%
    as.numeric()


  return(momento)

}
