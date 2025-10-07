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

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # --- Convertir a data.frame si no lo es ---
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # --- Determinar variable principal ---
  if (is.null(variable)) {
    numeric_vars <- names(x)[sapply(x, is.numeric)]
    if (length(numeric_vars) == 0) stop("No hay variables numéricas en el dataframe")
    variable <- numeric_vars[1]  # Tomamos la primera variable numérica
  }

  if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no válido")
    main_varname <- variable[1]
    x <- x[, variable[1], drop = FALSE]
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selección errónea de variables")
    main_varname <- names(x)[variable[1]]
    x <- x[, variable[1], drop = FALSE]
  } else {
    stop("El argumento 'variable' debe ser numérico o carácter")
  }

  # --- Validar que la variable es numérica ---
  if (!is.numeric(x[[1]])) stop("La variable seleccionada no es cuantitativa")

  # --- Caso sin pesos ---
  if (is.null(pesos)) {
    n_eff <- sum(!is.na(x[[1]]))
    if (n_eff < 2) {
      var_val <- NA_real_
    } else {
      factor <- if (tipo == "muestral") (n_eff - 1) / n_eff else 1
      var_val <- stats::var(x[[1]], na.rm = TRUE) * factor
    }
    var_val <- round(var_val, 4)
    names(var_val) <- paste0("varianza_", main_varname)
    return(var_val)
  }

  # --- Caso con pesos ---
  if (is.character(pesos)) {
    if (!pesos %in% names(x)) stop("Nombre de pesos no válido")
    pesos <- match(pesos, names(x))
  }

  x <- x[, c(1, pesos), drop = FALSE]

  # Eliminar filas con NA
  datos <- na.omit(x)
  if (nrow(datos) < 2) return(setNames(NA_real_, paste0("varianza_", main_varname)))

  # Media ponderada
  media_pond <- sum(datos[[1]] * datos[[2]]) / sum(datos[[2]])
  # Sumatorio ponderado
  sum_cuad <- sum((datos[[1]] - media_pond)^2 * datos[[2]])

  if (tipo == "muestral") {
    var_val <- sum_cuad / sum(datos[[2]])
  } else {
    var_val <- sum_cuad / (sum(datos[[2]]) - 1)
  }

  var_val <- round(var_val, 4)
  names(var_val) <- paste0("varianza_", main_varname)
  return(var_val)
}
