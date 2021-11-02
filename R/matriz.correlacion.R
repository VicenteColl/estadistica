#' @title Matriz de correlación.
#'
#' @description Obtiene la matriz de correlación (de Pearson) entre 2 o más variables cuantitativas.
#' @usage matriz.correlacion(x, variable = NULL, exportar = FALSE)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return La función devuelve la matriz de correlación lineal de las variables seleccionadas en un \code{data.frame}.
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
#' Se obtiene la matriz de correlación muestral:
#'
#' \if{html}{\figure{matrizcorrelacion.png}{options: width="50\%" alt="Figure: matrizcorrelacion.png"}}
#' \if{latex}{\figure{matrizcorrelacion.png}{options: scale=.5}}
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la matriz de correlació poblacional:
#'
#' \if{html}{\figure{matrizcorrelacionpob.png}{options: width="55\%" alt="Figure: matrizcorrelacionpob.png"}}
#' \if{latex}{\figure{matrizcorrelacionpob.png}{options: scale=.55}}
#'
#' @seealso \code{\link{correlacion}}, \code{\link{covarianza}},\code{\link{matriz.covar}}
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
#' matriz_cor <- matriz.correlacion(startup)
#'
#'
#' @importFrom stats na.omit cor
#' @import dplyr
#'
#' @export
matriz.correlacion <- function(x, variable = NULL, exportar = FALSE){

  if(is.null(variable)){

    x <- data.frame(x)
    varnames <- names(x)

  } else{

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

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la varianza, alguna variable que has seleccionado no es cuantitativa")
  }


  matriz_cor <- cor(x) %>%
    as.data.frame()
  names(matriz_cor) <- varnames
  row.names(matriz_cor) <- varnames

  if (exportar) {
    filename <- paste("Matriz de correlaci\u00f3n"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(matriz_cor, row.names = TRUE, file = filename)
  }

  return(matriz_cor)

}


