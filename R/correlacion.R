#' @title Coeficiente de correlación.
#'
#' @description Calcula el coeficiente de correlación de Pearson.
#' @usage correlacion(x, variable = NULL)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x solo tiene 2 variables (columnas), el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
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
#' El coeficiente de correlación muestral se obtiene a partir de la siguiente expresión:
#'
#' \figure{correlacion.png}{options: width="50\%" heigth="50\%"}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \figure{correlacion_pob.png}{options: width="30\%" heigth="30\%"}
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
correlacion <- function(x, variable = NULL){

  if(is.null(variable)){

    if(length(x) == 2){

      x <- data.frame(x)
      varnames <- names(x)

    } else{

      warning("Para obtener la matriz de varianzas-covarianzas utilizar la funcion matriz.var.covar()")
      stop("El conjunto de datos seleccionado tiene mas de 2 variables.")

    }

  } else if(length(variable) == 2){

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable

        } else{

          stop("Seleccion errronea de variables")

          }
      }

      if(is.character(variable)){

        if(all(variable %in% varnames)){

          variable = match(variable,varnames)

          } else {

            stop("El nombre de la variable no es valido")

          }

        }

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

    } else{

      warning("Para obtener la matriz de correlacion utilizar la funcion matriz.cor")
      stop("Para calcular la correlacion solo puedes seleccionar dos variables")

    }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse la correlacion, alguna variable que has seleccionado no es cuantitativa")

    }

    correlacion <- cor(x[1],x[2], use ="everything")
    correlacion <- as.data.frame(correlacion)

    names(correlacion) <- paste("correlacion_",varnames[1],"_",varnames[2],sep="")
    row.names(correlacion) <- NULL

  return(correlacion)

}
