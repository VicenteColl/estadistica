#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#' @usage cuantiles(x, variable = NULL, pesos = NULL, cortes = c(0.25,0.5,0.75))
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param cortes Vector con los puntos de corte a calcular. Por defecto se calcula el primer, segundo y tercer cuartil.
#'
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
#'
#' @references
#' Esteban García, J. et al. (2005). Estadística descriptiva y nociones de probabilidad. Thomson.
#'
#' @import
#'
#' @export
cuantiles <- function(x, variable = NULL, pesos = NULL, cortes = c(0.25,0.5,0.75)){

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    x <- x

  } else{

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable


      } else{

        stop("Seleccion erronea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es valido")
      }
    }

  }


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular los cuantiles a partir de la distribucion de frecuencias solo puedes seleccionar una variable y unos pesos")

    }

    if(is.numeric(pesos)){

      pesos <- pesos

    }

    if(is.character(pesos)){

      if(pesos %in% varnames){
        pesos = match(pesos,varnames)
      } else {
        stop("El nombre de los pesos no es valido")
      }
    }


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la media, alguna variable que has seleccionado no es cuantitativa")
  }

  cortes <- sort(cortes)

  if(is.null(pesos)){

    cuantiles <- apply(x,2,cuantiles.int,cortes=cortes)



  } else{

    x <- x
    cuantiles <- cuantiles.int(x[1],x[2],cortes)

  }

  cuantiles <- as.data.frame(cuantiles)
  names(cuantiles) <- paste("cuantiles_",varnames,sep="")
  row.names(cuantiles) <- paste(cortes*100,"%",sep="")

  return(cuantiles)

}
