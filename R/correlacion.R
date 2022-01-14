#' @title Coeficiente de correlación.
#'
#' @description Calcula el coeficiente de correlación de Pearson.
#' @usage correlacion(x, variable = NULL)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#'
#' @return Esta función devuelve el valor del coeficiente de correlación lineal en un objeto de la clase \code{data.frame}.
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
#' @details
#'
#' El coeficiente de correlación muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{correlacion.png}{options: width="50\%" alt="Figure: correlacion.png"}}
#' \if{latex}{\figure{correlacion.png}{options: width=5cm}}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \if{html}{\figure{correlacionpob.png}{options: width="30\%" alt="Figure: correlacionpob.png"}}
#' \if{latex}{\figure{correlacionpob.png}{options: width=4cm}}
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
correlacion <- function(x, variable = NULL){

  x <- data.frame(x)
  varnames <- names(x)


  if(is.null(variable)){

    if(length(x) == 2){

      x <- x

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

            stop("El nombre de la variable no es v\u00e1lido")

          }

        }

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

    } else{

      warning("Para obtener la matriz de correlaci\u00f3n utilizar la funci\u00f3n matriz.cor()")
      stop("Para calcular la correlaci\u00f3n solo puedes seleccionar dos variables")

    }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse la correlaci\u00f3n, alguna variable que has seleccionado no es cuantitativa")

    }

    correlacion <- cor(x[1],x[2], use ="everything")
    correlacion <- as.data.frame(correlacion)

    names(correlacion) <- paste("correlacion_",varnames[1],"_",varnames[2],sep="")
    row.names(correlacion) <- NULL

  return(correlacion)

}
