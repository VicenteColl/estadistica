#' @title Cuantiles (función intermedia).
#'
#' @description Función intermedia para el cálculo de los cuantiles.
#' @usage cuantiles.int(x, pesos = NULL, cortes = 0.5)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param cortes Vector con los puntos de corte a calcular. Por defecto se calcula el segundo cuartil (cortes = 0.5).

#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Olga Blasco-Blasco} (\email{olga.blasco@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Universidad de Valencia (Spain)
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#'
#' @import tidyverse
#'
#' @export
cuantiles.int <- function(x, pesos = NULL, cortes = 0.5){

  x <- as.data.frame(x)
  varnames <- names(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse los cuantiles, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    x <- drop_na(x)
    #y <- names(x)
    #names(x) <- varnames

    N <- nrow(x)
    tabla <- x %>% group_by(x) %>%
      count() %>%
      ungroup() %>%
      mutate(Ni = cumsum(n))

  } else{

    N <- sum(pesos)

    tabla <- data.frame(x,pesos) %>%
      na.omit
    #y <- names(tabla)
    names(tabla) <- c("x","pesos")

    tabla <- tabla %>%
      arrange(x) %>%
      mutate(Ni = cumsum(pesos))

  }


  cortes <- sort(cortes)

  cuantiles <- c()

  for(i in 1:length(cortes)){

    posicion <- min(which(tabla$Ni >= cortes[i]* N))

    if(cortes[i]* N == tabla$Ni[posicion]){

      cuantil <- mean(c(tabla$x[posicion],tabla$x[posicion+1]),na.rm=TRUE)

    } else {

      cuantil <- tabla$x[posicion]

    }

    cuantiles <- rbind(cuantiles,cuantil)


  }
  cuantiles <- as.data.frame(cuantiles)
  row.names(cuantiles) <- paste(cortes*100,"%",sep="")

  return(cuantiles)

}
