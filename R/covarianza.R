#' @title Covarianza.
#'
#' @description Calcula la covarianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x solo tiene 2 variables (columnas), el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la covarianza muestral (tipo = "muestral"). Si tipo = "cuasi", se calcula la cuasi-covarianza muestral.
#'
#' @return Esta función devuelve la covarianza en un objeto de la clase \code{vector}.
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
#' (1) La covarianza muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{covarianzamuestra.png}{options: style="width: 50\%;" alt="Figure: covarianzamuestra.png"}}
#' \if{latex}{\figure{covarianzamuestra.png}{options: width=6cm}}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la covarianza a partir de la expresión:
#'
#' \if{html}{\figure{covarianzacuasi.png}{options: style="width: 50\%;" alt="Figure: covarianzacuasi.png"}}
#' \if{latex}{\figure{covarianzacuasi.png}{options: width=6cm}}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la covarianza poblacional:
#'
#' \if{html}{\figure{covarianzapob.png}{options:style="width: 50\%;" alt="Figure: covarianzapob.png"}}
#' \if{latex}{\figure{covarianzapob.png}{options: width=6cm}}

#' @seealso \code{\link{varianza}}, \code{\link{desviacion}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom stats cov
#' @import dplyr
#'
#' @export
covarianza <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  if (!is.data.frame(x)) x <- data.frame(x)

  # --- Seleccion de variables ---
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)][1:2]
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables.")
    varnames <- names(x)[variable]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no v\u00e1lido.")
    varnames <- variable
  } else {
    stop("El argumento 'variable' debe ser num\u00e9ico o car\u00e1cter.")
  }

  # --- Caso SIN pesos ---
  if (is.null(pesos)) {
    x_sel <- x[, varnames, drop = FALSE]
    x_sel <- na.omit(x_sel)
    n_eff <- nrow(x_sel)
    if (n_eff < 2) stop("No hay suficientes observaciones completas.")

    cov_val <- stats::cov(x_sel[[1]], x_sel[[2]], use = "complete.obs")
    # R ya devuelve la version cuasi (divisor n-1)
    if (tipo == "muestral") cov_val <- cov_val * ((n_eff - 1) / n_eff)

    result <- as.data.frame(round(as.numeric(cov_val), 4))
    names(result) <- paste0(varnames[1], "_", varnames[2])
    row.names(result) <- NULL

    class(result) <- c("resumen", "data.frame")
    return(result)
  }


  # --- Caso CON pesos ---
  if (length(pesos) != 1) stop("Solo se admite una variable de pesos.")

  # Determinar la columna de pesos desde el data.frame ORIGINAL
  if (is.character(pesos)) {
    if (!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido.")
    peso_col <- pesos
  } else if (is.numeric(pesos)) {
    if (pesos > ncol(x)) stop("Indice de columna de pesos fuera de rango del conjunto de datos.")
    peso_col <- names(x)[pesos]
  } else {
    stop("El argumento 'pesos' debe ser numr\u00e9ico o de tipo car\u00e1cter.")
  }

  # Crear nuevo data.frame con las variables y los pesos
  x_sel <- x[, c(varnames, peso_col), drop = FALSE]
  names(x_sel) <- c("var1", "var2", "pesos")

  # Eliminar NA
  x_sel <- na.omit(x_sel)
  n_eff <- nrow(x_sel)
  if (n_eff < 2) stop("No hay suficientes observaciones completas para calcular la covarianza ponderada.")

  # Calcular medias ponderadas
  media1 <- sum(x_sel$var1 * x_sel$pesos, na.rm = TRUE) / sum(x_sel$pesos, na.rm = TRUE)
  media2 <- sum(x_sel$var2 * x_sel$pesos, na.rm = TRUE) / sum(x_sel$pesos, na.rm = TRUE)

  # Covarianza ponderada
  sum_cuad <- sum((x_sel$var1 - media1) * (x_sel$var2 - media2) * x_sel$pesos, na.rm = TRUE)

  if (tipo == "muestral") {
    cov_val <- sum_cuad / sum(x_sel$pesos, na.rm = TRUE)
  } else {
    cov_val <- sum_cuad / (sum(x_sel$pesos, na.rm = TRUE) - 1)
  }

  # Resultado como df
  result <- as.data.frame(round(as.numeric(cov_val), 4))
  names(result) <- paste0(varnames[1], "_", varnames[2])
  row.names(result) <- NULL

  class(result) <- c("resumen", "data.frame")
  return(result)
}
