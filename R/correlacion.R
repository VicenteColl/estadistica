#' @title Coeficiente de correlación.
#'
#' @description Calcula el coeficiente de correlación de Pearson.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcorrelacion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcorrelacion.png}{options: width=3cm}}
#'
#' @usage correlacion(x, variable = NULL, pesos=NULL)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Esta función devuelve el valor del coeficiente de correlación lineal en un objeto de la clase \code{vector}.
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
#' @details
#'
#' El coeficiente de correlación muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{correlacion.png}{options: width="50\%" alt="Figure: correlacion.png"}}
#' \if{latex}{\figure{correlacion.png}{options: width=5.5cm}}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \if{html}{\figure{correlacionpob.png}{options: width="30\%" alt="Figure: correlacionpob.png"}}
#' \if{latex}{\figure{correlacionpob.png}{options: width=3.5cm}}
#'
#' @seealso \code{\link{matriz.correlacion}}, \code{\link{covarianza}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' correlacion1 <- correlacion(startup[,c(1,3)])
#' correlacion2 <- correlacion(startup,variable=c(1,3))
#'
#' @importFrom stats cor
#' @import dplyr
#'
#' @export
correlacion <- function(x, variable = NULL, pesos=NULL){

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){

      varcuan <-  names(x[unlist(lapply(x, is.numeric))])
      seleccion = match(varcuan,varnames)
      x <- x[seleccion]
      varnames <- varcuan
    } else{
      warning("Para obtener la matriz de correlaci\u00f3n utilizar la funcion matriz.correlacion()")
      stop("El conjunto de datos seleccionado tiene mas de 2 variables.")
    }

  } else if(length(variable) == 2){

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

          stop("El nombre de la variable no es v\u00edlido")

        }

      }

    } else{

      warning("Para obtener la matriz de varianzas-covarianzas utilizar la funci\u00f3n matriz.covar()")
      stop("Para calcular la covarianza solo puedes seleccionar dos variables")
    }

  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 3){

      stop("Para calcular la covarianza a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar dos variables y unos pesos")

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

    stop("No puede calcularse la covarianza, alguna variable que has seleccionado no es cuantitativa")

  }

  if(is.null(pesos)){
    correlacion <- cor(x[1],x[2], use ="everything")
    correlacion <- as.data.frame(correlacion)
  } else{
    x <- x %>%
    na.omit %>%
    rename(variable1 = varnames[1], variable2 = varnames[2], pesos = varnames[3])

  correlacion <- x %>%
    summarize(correlacion = covarianza(x,variable=c(1,2),3) / (desviacion(x,1,3)*desviacion(x,2,3)))
  }

  correlacion <- as.numeric(correlacion) %>% round(4)
  names(correlacion) <- paste("correlacion_",varnames[1],"_",varnames[2],sep="")
  row.names(correlacion) <- NULL

  return(correlacion)

}
