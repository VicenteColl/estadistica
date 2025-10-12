#' @title Mediana.
#'
#' @description Calcula la mediana.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
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

  var_name <- deparse(substitute(x))

  # Convertir a data.frame
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # --- Selecci\u00f3n de variables ---
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
    varnames <- names(x)[variable]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no v\u00e1lido")
    varnames <- variable
  } else {
    stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
  }

  # Subconjunto de variables seleccionadas
  x_sel <- x[, varnames, drop = FALSE]

  # --- Manejo de pesos ---
  if (!is.null(pesos)) {
    if (length(varnames) > 1)
      stop("Solo puedes seleccionar una variable con pesos")

    # Buscar columna de pesos en el data.frame original, no en x_sel
    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido")
      pesos_col <- pesos
    } else if (is.numeric(pesos)) {
      if (pesos > ncol(x)) stop("Selecci\u00f3n de pesos no v\u00e1lida")
      pesos_col <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    datos <- na.omit(data.frame(variable = x[[varnames]], pesos = x[[pesos_col]]))
    if (nrow(datos) == 0) return(data.frame(matrix(NA, ncol = 1, dimnames = list(NULL, varnames))))

    result <- .mediana.int(datos$variable, datos$pesos)
    result_df <- data.frame(result)
    colnames(result_df) <- varnames

    class(result_df) <- c("resumen", class(result_df))
    return(round(result_df, 4))
  }

  # --- Mediana simple ---
  result <- sapply(x_sel, .mediana.int)
  result_df <- as.data.frame(t(result))  # convertimos a data.frame con columnas como variables
  colnames(result_df) <- varnames
  rownames(result_df) <- NULL

  class(result_df) <- c("resumen", class(result_df))
  return(result_df)
}
