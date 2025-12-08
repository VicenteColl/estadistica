#' @title Coeficiente de correlación.
#'
#' @description Calcula el coeficiente de correlación de Pearson.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcorrelacion.png}{options: style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcorrelacion.png}{options: width=3cm}}
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
#' \if{html}{\figure{correlacion.png}{options: style="width: 50\%;" alt="Figure: correlacion.png"}}
#' \if{latex}{\figure{correlacion.png}{options: width=5.5cm}}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \if{html}{\figure{correlacionpob.png}{options: style="width: 30\%;" alt="Figure: correlacionpob.png"}}
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
correlacion <- function(x,
                        variable = NULL,
                        pesos = NULL) {

  # Asegurar data.frame
  x <- as.data.frame(x)
  varnames <- names(x)

  # --- Seleccion de variables ---
  if (is.null(variable)) {
    # Si solo hay 2 variables numericas, las selecciona
    varcuan <- names(x)[sapply(x, is.numeric)]
    if (length(varcuan) != 2) {
      stop("Debes seleccionar exactamente 2 variables para calcular la correlaci\u00f3n.")
    }
    variable <- match(varcuan, varnames)
  } else {
    if (is.character(variable)) {
      if (!all(variable %in% varnames)) stop("El nombre de la variable no es v\u00e1lido")
      variable <- match(variable, varnames)
    } else if (is.numeric(variable)) {
      if (any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
    } else {
      stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }
    if (length(variable) != 2) stop("Solo se pueden seleccionar 2 variables")
  }

  # Subconjunto de variables
  x_sel <- x[, variable, drop = FALSE]
  varnames <- names(x_sel)

  # Comprobar el tipo de variables
  if (!all(sapply(x_sel, is.numeric))) stop("Las variables deben ser cuantitativas")

  # --- Correlacion ---
  if (is.null(pesos)) {
    # Correlacion simple eliminando NAs
    corr_val <- cor(x_sel[[1]], x_sel[[2]], use = "complete.obs")
  } else {
    # Pesos
    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("El nombre de los pesos no es v\u00e1lido")
      pesos <- match(pesos, names(x))
    } else if (is.numeric(pesos)) {
      if (any(pesos > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de pesos")
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }
    x_sel <- x[, c(variable, pesos)]
    colnames(x_sel) <- c("var1", "var2", "pesos")
    x_sel <- na.omit(x_sel)

    # Correlacion ponderada
    corr_val <- sum((x_sel$var1 - sum(x_sel$var1 * x_sel$pesos) / sum(x_sel$pesos)) *
                      (x_sel$var2 - sum(x_sel$var2 * x_sel$pesos) / sum(x_sel$pesos)) * x_sel$pesos) /
      sqrt(sum((x_sel$var1 - sum(x_sel$var1 * x_sel$pesos) / sum(x_sel$pesos))^2 * x_sel$pesos) *
             sum((x_sel$var2 - sum(x_sel$var2 * x_sel$pesos) / sum(x_sel$pesos))^2 * x_sel$pesos))
  }

  # --- Devolver dataframe ---
  result <- data.frame(corr_val)
  names(result) <- paste0(varnames[1],"_",varnames[2])

  return(result)
}
