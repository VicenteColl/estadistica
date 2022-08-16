#' @title Matriz de varianzas y covarianzas.
#'
#' @description Obtiene la matriz de varianzas y covarianzas.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @usage matriz.covar(x,
#'               variable = NULL,
#'               tipo = c("muestral","cuasi"),
#'               exportar = FALSE)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param tipo Es un carácter. Por defecto de calcula la matriz de varianzas y covarianzas muestrales (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la matriz de cuasi-varianzas y cuasi-covarianzas muestrales.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return La función devuelve la matriz de varianzas-covarianzas (muestrales, por defecto) de las variables seleccionadas en un \code{data.frame}.
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
#' (1) Se obtiene la matriz de varianzas y covarianzas muestrales:
#'
#' \if{html}{\figure{matrizvarcovmuestra.png}{options: width="50\%" alt="Figure: matrizvarcovmuestra.png"}}
#' \if{latex}{\figure{matrizvarcovmuestra.png}{options: width=8cm}}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) facilitan la matriz de cuasi-varianzas y cuasi-covarianzas muestrales:
#'
#' \if{html}{\figure{matrizvarcovcuasi.png}{options: width="55\%" alt="Figure: matrizvarcovcuasi.png"}}
#' \if{latex}{\figure{matrizvarcovcuasi.png}{options: width=8cm}}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la matriz de varianzas y covarianzas poblacional:
#'
#' \if{html}{\figure{matrizvarcovpob.png}{options: width="55\%" alt="Figure: matrizvarcovpob.png"}}
#' \if{latex}{\figure{matrizvarcovpob.png}{options: width=8cm}}
#'
#' @seealso \code{\link{varianza}}, \code{\link{desviacion}}
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
#' matriz_covarianzas1 <- matriz.covar(startup)
#' matriz_covarianzas2 <- matriz.covar(startup, tipo= "cuasi")
#'
#' @importFrom stats na.omit cov
#' @import dplyr
#'
#' @export
matriz.covar <- function(x, variable = NULL,
                         tipo = c("muestral","cuasi"),
                         exportar = FALSE){


  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    varcuan <-  names(x[unlist(lapply(x, is.numeric))])
    seleccion = match(varcuan,varnames)
    x <- x[seleccion]
    varnames <- varcuan

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

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la matriz de varianzas-covarianzas, alguna variable que has seleccionado no es cuantitativa")
  }

  if(tipo == "muestral"){

    n <- nrow(x)
    factor = (n-1)/n

  } else{

    factor <- 1
  }

  matriz_covar <- factor * var(x, na.rm = TRUE) %>%
    as.matrix()
  colnames(matriz_covar) <- varnames
  row.names(matriz_covar) <- varnames

  if (exportar) {
    filename <- paste("Matriz de covarianzas"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(matriz_covar, rowNames = TRUE, file = filename)
  }

  return(matriz_covar)

}


