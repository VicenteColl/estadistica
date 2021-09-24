#' @title Moda.
#'
#' @description Calcula la moda.
#' @usage moda(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Olga Blasco-Blasco} (\email{olga.blasco@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr
#'
#' @export
moda <- function(x, variable = NULL, pesos = NULL){

  x <- data.frame(x)
  varnames <- names(x)

  if(!is.null(variable)){

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable

      } else{

        stop("Selecci\u00f3n err\u00f3nea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }

  }


  if(length(x) == 1 ) {

      variable.sel <- varnames

      x <- x %>%
        na.omit

  }


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular la moda a partir de la distribuci\u00fn de frecuencias solo puedes seleccionar una variable y unos pesos")

    }

    if(is.numeric(pesos)){

      pesos <- pesos

    }


    if(is.character(pesos)){

      if(pesos %in% varnames){
        pesos = match(pesos,varnames)
      } else {
        stop("El nombre de los pesos no es v\u00e1lido")
      }
    }

    if(pesos == variable){

      stop("Has seleccionado la misma columna del dataframe para la variable y los pesos")

    }


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[c(variable,pesos)]

  }

  clase <- sapply(x, class)


  if (!all(clase %in% c("numeric","integer","factor","logic"))) {
    stop("No se puede calcular la moda, comprueba el tipo de variable seleccionada")
  }

  if(is.null(pesos)){

    moda <- table(x)
    moda <- names(moda)[which(moda==max(moda))]
    moda <- as.numeric(moda)

    if(length(moda)==nrow(x)){
      warning("Esta variable no tiene moda, todos los valores tienen la misma frecuencia")
      moda <- NA
    }

  } else{

    moda <- x %>%
      na.omit %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      group_by(variable2) %>%
      summarize(frecuencia = sum(pesos), .groups = 'drop') %>%
      arrange(desc(frecuencia)) %>%
      as.data.frame()

    valores_distintos <- unique(moda$variable2)

    moda <- moda %>%
      summarize(moda = variable2[which(frecuencia == max(frecuencia))]) %>%
      as.data.frame()

    if(nrow(moda) == length(valores_distintos)){
      warning("Esta variable no tiene moda, todos los valores tienen la misma frecuencia")
      moda <- NA
    }

  }

  return(moda)

}

