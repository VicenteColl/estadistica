#' @title Varianza.
#'
#' @description Calcula la varianza.
#' @usage varianza(x, variable = NULL, pesos = NULL, tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la varianza muestral (tipo = "muestral"). Si tipo = "cuasi", se calcula la cuasivarianza muestral.
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
#' Universidad de Valencia (España)
#'
#' @details
#'
#' La expresión de la varianza muestral es:
#'
#' \figure{varianza.jpg}{options: width="35\%" heigth="35\%"}
#'
#' La varianza muestral es el estimador máximo verosímil de la varianza de una población normal
#'
#' La expresión de la cuasi-varianza muestral es:
#'
#' \figure{cuasivarianza.jpg}{options: width="35\%" heigth="35\%"}
#'
#' La cuasivarianza muestral es un estimador insesgado de la varianza poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la varianza poblacional:
#'
#' \figure{varianza_pob.jpg}{options: width="35\%" heigth="35\%"}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import
#'
#' @export
varianza <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral","cuasi")){

  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    x <- x

  } else{

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


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular la varianza a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")

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


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[c(variable,pesos)]

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la varianza, alguna variable que has seleccionado no es cuantitativa")
  }

  tipo_varianza <- c("muestral","cuasi")

  tipo <- tolower(tipo)

  if(!(tipo %in% tipo_varianza)){

    stop("Indica si quieres calcular la varianza muestral o la cuasi-varianza")

  }

  if(is.null(pesos) & tipo == "muestral"){

    n <- nrow(x)
    factor = (n-1)/n

  } else{

    factor <- 1
  }

  if(is.null(pesos)){

    varianza <- apply(x,2,var,na.rm=TRUE)
    varianza <- factor * varianza
    varianza <- as.data.frame(t(varianza))

  } else{

    varianza <- x %>%
      na.omit %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      dplyr::mutate(media = as.numeric(media(x,variable=1,pesos=2)),
                    sumatorio = (variable2-media)^2*pesos)

    varnames <- varnames[1]

    if(tipo == "muestral"){

      varianza <- varianza %>% summarize(varianza = sum(sumatorio)/sum(pesos))

    } else{

      varianza <- varianza %>% summarize(varianza = sum(sumatorio)/(sum(pesos)-1))


    }

    names(varianza) <- paste("varianza_",varnames,sep="")

  }


  return(varianza)

}
