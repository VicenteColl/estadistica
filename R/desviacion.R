#' @title Desviación típica.
#'
#' @description Calcula la desviación típica.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la desviación típica muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasi-desviación típica muestral.
#'
#' @return Esta función devuelve un objeto de la clase \code{vector}. Si \code{tipo="muestral"}, devuelve la desviación típica muestral. Si \code{tipo="cuasi"}, devuelve la cuasi-desviación típica muestral.
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
#' (1) La expresión de la de la desviación típica muestral es:
#'
#' \if{html}{\figure{desviacion.png}{options: style="width: 40\%;"}}
#' \if{latex}{\figure{desviacion.png}{options: width=5cm}}
#'
#' La desviación típica muestral así definida es el estimador máximo verosímil de la desviación típica de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasidesviacion.png}{options: style="width: 40\%;"}}
#' \if{latex}{\figure{cuasidesviacion.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-desviación típica muestral y es un estimador insesgado de la desviación típica poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la desviación típica poblacional:
#'
#' \if{html}{\figure{desviacionpob.png}{options: style="width: 40\%;"}}
#' \if{latex}{\figure{desviacionpob.png}{options: width=5cm}}
#'
#' @seealso \code{\link{media}}, \code{\link{varianza}}, \code{\link{coeficiente.variacion}}
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
#' desviacion1 <- desviacion(startup[1])
#' desviaciona2 <- desviacion(startup,variable=1)
#' desviacion3 <- desviacion(startup,variable=1, tipo="cuasi")
#'
#' @importFrom stats sd na.omit
#'
#' @export
desviacion <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # --- Asegurar data.frame ---
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
    stop("No puede calcularse la desviaci\u00f3n: alguna variable no es cuantitativa")
  }

  # --- Si no hay pesos, calculamos desviacion simple ---
  if (is.null(pesos)) {
    calcular_desv <- function(col) {
      n_eff <- sum(!is.na(col))
      if (n_eff < 2) return(NA_real_)
      factor <- if (tipo == "muestral") sqrt((n_eff - 1) / n_eff) else 1
      round(sqrt(stats::var(col, na.rm = TRUE)) * factor, 4)
    }

    desv_val <- sapply(x_sel, calcular_desv)
    names(desv_val) <- names(x_sel)

    desv_df <- as.data.frame(t(desv_val))

    return(desv_df)
  }

  # --- Si hay pesos ---
  if (length(pesos) != 1 || length(varnames) != 1) {
    stop("Para desviaci\u00f3n ponderada solo puedes seleccionar una variable y un vector de pesos")
  }

  # Determinar la columna de pesos
  if (is.character(pesos)) {
    if (!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido")
    pesos_col <- x[[pesos]]
  } else if (is.numeric(pesos)) {
    if (pesos > ncol(x)) stop("Indice de pesos inv\u00e1lido")
    pesos_col <- x[[pesos]]
  } else {
    stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
  }

  datos <- na.omit(data.frame(variable = x_sel[[1]], pesos = pesos_col))
  media_pond <- sum(datos$variable * datos$pesos) / sum(datos$pesos)
  sum_cuad <- sum((datos$variable - media_pond)^2 * datos$pesos)

  desv_val <- if (tipo == "muestral") {
    sqrt(sum_cuad / sum(datos$pesos))
  } else {
    sqrt(sum_cuad / (sum(datos$pesos) - 1))
  }

  # --- Convertir a data.frame con una fila ---
  desv_df <- as.data.frame(t(desv_val))
  names(desv_df) <- varnames


  # desv_val <- round(desv_val, 4)
  # names(desv_val) <- paste0("desviacion_", varnames[1])

  class(desv_df) <- c("resumen", class(desv_df))

  return(desv_df)
}
