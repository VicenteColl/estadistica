#' @title Varianza.
#'
#' @description Calcula la varianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la varianza muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasivarianza muestral.
#'
#' @return Esta función devuelve un objeto de la clase \code{vector}. Si \code{tipo="muestral"}, devuelve la varianza muestral. Si \code{tipo="cuasi"}, devuelve la cuasi-varianza muestral.
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
#' (1) La expresión de la varianza muestral es:
#'
#' \if{html}{\figure{varianza.png}{options: style="width: 40\%;" alt="Figure: varianza.png"}}
#' \if{latex}{\figure{varianza.png}{options: width=5cm}}
#'
#' La varianza muestral así definida es el estimador máximo verosímil de la varianza de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasivarianza.png}{options: style="width: 40\%;" alt="Figure: cuasivarianza.png"}}
#' \if{latex}{\figure{cuasivarianza.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-varianza muestral y es un estimador insesgado de la varianza poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la varianza poblacional:
#'
#'
#' \if{html}{\figure{varianzapob.png}{options: style="width: 40\%;" alt="Figure: varianzapob.png"}}
#' \if{latex}{\figure{varianzapob.png}{options: width=5cm}}
#'
#' @seealso \code{\link{media}}, \code{\link{desviacion}}, \code{\link{coeficiente.variacion}}
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
#' varianza1 <- varianza(startup[1])
#' varianza2 <- varianza(startup,variable=1)
#' varianza3 <- varianza(startup,variable=1, tipo="cuasi")
#'
#' @importFrom stats var
#'
#' @export
varianza <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  if (!is.data.frame(x)) x <- data.frame(x)

  # --- Seleccion de variables ---
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

  x_sel <- x[, varnames, drop = FALSE]

  # --- Comprobar tipo de variables ---
  if (!all(sapply(x_sel, is.numeric))) {
    stop("No puede calcularse la varianza: alguna variable seleccionada no es cuantitativa")
  }

  # --- Si no hay pesos ---
  if (is.null(pesos)) {

    x_sel <- x[, varnames, drop = FALSE]

    # Comprobacion tipo de variable
    if (!all(sapply(x_sel, is.numeric))) {
      stop("No puede calcularse la varianza: alguna variable seleccionada no es cuantitativa")
    }

    calcular_var <- function(col) {
      n_eff <- sum(!is.na(col))
      if (n_eff < 2) return(NA_real_)
      factor <- if (tipo == "muestral") (n_eff - 1) / n_eff else 1
      stats::var(col, na.rm = TRUE) * factor
    }

    var_val <- sapply(x_sel, calcular_var)
    var_df <- as.data.frame(t(var_val))
    names(var_df) <- varnames

  } else {
    # --- Si hay pesos ---

    # Determinar indices o nombres
    if (is.character(pesos)) {
      if (!(pesos %in% names(x))) stop("El nombre de los pesos no es v\u00e1lido")
      pesos_idx <- match(pesos, names(x))
    } else if (is.numeric(pesos)) {
      if (pesos > ncol(x)) stop("Selecci\u00f3n err\u00f3nea de pesos")
      pesos_idx <- pesos
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    if (length(varnames) > 1) {
      stop("Solo puede calcularse la varianza ponderada para una variable a la vez")
    }

    variable_idx <- match(varnames, names(x))
    x_sel <- x[, c(variable_idx, pesos_idx)]
    names(x_sel) <- c("variable2", "pesos")

    if (!is.numeric(x_sel$variable2) || !is.numeric(x_sel$pesos)) {
      stop("Tanto la variable como los pesos deben ser num\u00e9ricos")
    }

    x_sel <- na.omit(x_sel)

    # Calcular media ponderada
    media_pond <- sum(x_sel$variable2 * x_sel$pesos) / sum(x_sel$pesos)

    # Suma ponderada de cuadrados
    sumatorio <- (x_sel$variable2 - media_pond)^2 * x_sel$pesos

    # Denominador segun tipo
    if (tipo == "muestral") {
      denom <- sum(x_sel$pesos)
    } else {
      denom <- sum(x_sel$pesos) - 1
    }

    var_val <- sum(sumatorio) / denom

    var_df <- data.frame(varianza = var_val)
    names(var_df) <- varnames
  }

  class(var_df) <- c("resumen", class(var_df))
  return(var_df)
}

