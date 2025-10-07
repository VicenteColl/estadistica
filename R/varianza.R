#' @title Varianza.
#'
#' @description Calcula la varianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#' @usage varianza(x,
#'                 variable = NULL,
#'                 pesos = NULL,
#'                 tipo = c("muestral","cuasi"))
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
#' \if{html}{\figure{varianza.png}{options: width="40\%" alt="Figure: varianza.png"}}
#' \if{latex}{\figure{varianza.png}{options: width=5cm}}
#'
#' La varianza muestral así definida es el estimador máximo verosímil de la varianza de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasivarianza.png}{options: width="40\%" alt="Figure: cuasivarianza.png"}}
#' \if{latex}{\figure{cuasivarianza.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-varianza muestral y es un estimador insesgado de la varianza poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la varianza poblacional:
#'
#'
#' \if{html}{\figure{varianzapob.png}{options: width="40\%" alt="Figure: varianzapob.png"}}
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
varianza <- function(x,
                     variable = NULL,
                     pesos = NULL,
                     tipo = c("muestral", "cuasi")) {

  # Capturar nombre original
  var_name <- deparse(substitute(x))
  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # --- Prepara estructura ---
  if (is.data.frame(x) || is.list(x)) {
    x <- as.data.frame(x)
  } else {
    if (grepl("\\$", var_name)) {
      varnames <- sub(".*\\$", "", var_name)
    } else {
      varnames <- "variable"
    }
    x <- data.frame(x)
    names(x) <- varnames
  }

  # --- Determina la variable principal ---
  if (is.null(variable)) {
    varnumeric <- names(x)[sapply(x, is.numeric)]
    variable <- match(varnumeric, names(x))
  } else if (is.character(variable)) {
    if (!variable %in% names(x)) stop("Nombre de variable no v\u00e1lido")
    variable <- match(variable, names(x))
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
  }

  main_varname <- names(x)[variable]  # nombre definitivo de la variable

  # --- Pesos ---
  if (!is.null(pesos)) {
    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("El nombre de los pesos no es v\u00e1lido")
      pesos <- match(pesos, names(x))
    }
    x <- x[, c(variable, pesos), drop = FALSE]
    varnames <- names(x)
  } else {
    x <- x[, variable, drop = FALSE]
    varnames <- names(x)
  }

  # --- Comprobar tipo de variable ---
  if (!all(sapply(x, is.numeric))) {
    stop("No puede calcularse la varianza: alguna variable seleccionada no es cuantitativa")
  }

  # --- Caso sin pesos ---
  if (is.null(pesos)) {

    n_eff <- sum(!is.na(x[[1]]))
    if (n_eff < 2) return(NA_real_)

    factor <- if (tipo == "muestral") (n_eff - 1) / n_eff else 1

    var_val <- stats::var(x[[1]], na.rm = TRUE) * factor
    var_val <- round(var_val, 4)
    names(var_val) <- paste0("varianza_", main_varname)

  } else {
    # --- Caso con pesos ---
    var_val <- x %>%
      na.omit() %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      mutate(media = as.numeric(media(x, variable = 1, pesos = 2)),
             sumatorio = (variable2 - media)^2 * pesos)

    if (tipo == "muestral") {
      var_val <- var_val %>%
        summarize(varianza = sum(sumatorio) / sum(pesos))
    } else {
      var_val <- var_val %>%
        summarize(varianza = sum(sumatorio) / (sum(pesos) - 1))
    }

    var_val <- var_val$varianza
    var_val <- round(var_val, 4)
    names(var_val) <- paste0("varianza_", main_varname)
  }

  return(var_val)
}

