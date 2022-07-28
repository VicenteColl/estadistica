#' @title Momento central.
#'
#' @description Calcula los momentos centrales respecto de la media.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrforma.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrforma.png}{options: width=3cm}}
#'
#' @usage momento.central(x, orden)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param orden Es un valor numérico que representa el orden del momento central (orden = {1,2,3,4,...})
#'
#' @return Devuelve el valor de momento central de orden seleccionado
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr
#'
#' @export
momento.central <- function(x, orden){

  if(is.numeric(x)){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)
  names(x) <- varnames


  varcuan <-  names(x)[which(sapply(x[varnames], is.numeric))]
  #seleccion = match(varcuan,varnames)
  x <- x[varcuan]
  varnames <- varcuan

  orden <- as.integer(orden)

  if(!is.integer(orden)){

    stop("El orden del momento central debe ser un valor num\u00e9rico entero")

  }

  x <- data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }

  momento_vacio <- vector("list",length=length(x))

  for(i in 1:length(x)){

    momento <- x[i] %>% na.omit %>%
      mutate(media_x = media(x[i]),
             momento = (x[i]-media_x)^orden) %>%
      summarize(momento = sum(momento)/n()) %>%
      as.numeric()

    momento_vacio[[i]] <- momento

  }

  max_long <-  max(lengths(momento_vacio))
  momento <- sapply(momento_vacio, "[", seq_len(max_long)) %>%
    t() %>%
    as.numeric()
  names(momento) <- varnames

  return(momento)

}
