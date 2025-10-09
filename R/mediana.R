#' @title Mediana.
#'
#' @description Calcula la mediana.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @usage mediana(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la mediana de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la mediana de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' La mediana se obtiene a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{mediana.png}{options: width="80\%" alt="Figure: mediana.png"}}
#' \if{latex}{\figure{mediana.png}{options: scale=.8}}
#'
#' donde: Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' @seealso \code{\link{media}}, \code{\link{cuantiles}}
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
#' mediana1 <- mediana(startup[1])
#' mediana2 <- mediana(startup,variable=1)
#' mediana3 <- mediana(salarios2018,variable=6,pesos=7)
#'
#' @import dplyr
#'
#' @export
mediana <- function(x, variable = NULL, pesos = NULL) {

  # Capturar el nombre original si es un vector
  var_name <- deparse(substitute(x))

  # Manejo de nombres para diferentes tipos de entrada
  if (is.data.frame(x) || is.list(x)) {
    # Para data.frames/listas
    original_names <- names(x)
    x <- as.data.frame(x)

    if (is.null(variable)) {
      varnames <- names(x)[sapply(x, is.numeric)]
    } else {
      if (is.numeric(variable)) {
        varnames <- names(x)[variable]
      } else {
        varnames <- variable
      }
    }
  } else {
    # Para vectores
    if (grepl("\\$", var_name)) {
      # Si es de tipo dataframe$columna
      varnames <- sub(".*\\$", "", var_name)
    } else {
      # Si es un vector simple
      varnames <- "variable"
    }
    x <- data.frame(x)
    names(x) <- varnames
    original_names <- varnames
  }

  # Seleccion de variables
  if(!is.null(variable)) {
    if(is.character(variable)) {
      if(!all(variable %in% names(x))) stop("Nombre de variable no v\u00e1lido")
      x <- x[, variable, drop = FALSE]
    } else if(is.numeric(variable)) {
      if(any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
      x <- x[, variable, drop = FALSE]
    } else {
      stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }
  } else {
    # Seleccionar solo columnas numericas si no se especifica variable
    x <- x[, sapply(x, is.numeric), drop = FALSE]
    if(ncol(x) == 0) stop("No hay variables numéricas para calcular la mediana")
  }

  # Manejo de pesos
  if(!is.null(pesos)) {
    if(ncol(x) > 1) stop("Solo puedes seleccionar una variable con pesos")

    if(is.character(pesos)) {
      if(!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido")
       pesos_col <- pesos
    } else if(is.numeric(pesos)) {
      if(pesos > ncol(x)) stop("Selecci\u00f3n de pesos no v\u00e1lida")
      pesos_col <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    # Calcular mediana ponderada
    datos <- na.omit(x)
    if(nrow(datos) == 0) return(NA)

    result <- .mediana.int(datos[[1]], datos[[pesos_col]])
    names(result) <- paste0("mediana_", names(x)[1])
    return(round(result, 4))
  }

  # Calcular medianas simples
  result <- sapply(x, .mediana.int)
  names(result) <- paste0("mediana_", names(x))

  class(result) <- c("resumen", class(result))


  return(result)
}
