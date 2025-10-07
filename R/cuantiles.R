#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcuantiles.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcuantiles.png}{options: width=3cm}}
#'
#' @usage cuantiles(x,
#'                  variable = NULL,
#'                  pesos = NULL,
#'                  cortes = c(0.25,0.5,0.75),
#'                  exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param cortes Vector con los puntos de corte a calcular. Por defecto se calcula el primer, segundo y tercer cuartil.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Si \code{pesos = NULL}, la función devuelve los cuantiles de todas las variables seleccionadas en un objeto de tipo \code{data.frame}. En caso contrario, devuelve los cuantiles de la variable para la que se ha facilitado la distribución de frecuencias.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' @details
#'
#' Los cuantiles se obtienen a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{cuantiles.png}{options: width="85\%" alt="Figure: cuantiles.png"}}
#' \if{latex}{\figure{cuantiles.png}{options: scale=.85}}
#'
#' Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' cuartiles: s=1,2,3 y k=4
#'
#' deciles: s= 1,2,...,9 y k=10
#'
#' percentiles: s=1,2,...,99 y k=100
#'
#' @seealso \code{\link{media}}, \code{\link{mediana}}
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
#' cuantiles1 <- cuantiles(startup[1])
#' cuantiles2 <- cuantiles(startup,variable=1,cortes=seq(0.1,0.9,0.1))
#' cuantiles3 <- cuantiles(salarios2018,variable=6,pesos=7 )
#'
#' @export
cuantiles <- function(x,
                      variable = NULL,
                      pesos = NULL,
                      cortes = c(0.25, 0.5, 0.75),
                      exportar = FALSE) {

  # --- Convertir a data.frame ---
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # --- Selección de variable ---
  if (is.null(variable)) {
    numeric_vars <- names(x)[sapply(x, is.numeric)]
    if (length(numeric_vars) == 0) stop("No hay variables numéricas en el dataframe")
    variable <- numeric_vars[1]
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

  if (!is.numeric(x[[1]])) stop("La variable seleccionada no es cuantitativa")

  cortes <- sort(cortes)

  # --- Caso sin pesos ---
  if (is.null(pesos)) {
    cuantiles_val <- stats::quantile(x[[1]], probs = cortes, na.rm = TRUE, names = FALSE)
    cuantiles_val <- round(cuantiles_val, 4)
  } else {
    # --- Caso ponderado ---
    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("Nombre de pesos no válido")
      pesos <- match(pesos, names(x))
    }
    datos <- x[, c(1, pesos)]
    datos <- na.omit(datos)
    if (nrow(datos) < 1) return(setNames(rep(NA_real_, length(cortes)), paste0("cuantiles_", main_varname)))

    # Calcular cuantiles ponderados
    ord <- order(datos[[1]])
    x_ord <- datos[[1]][ord]
    w_ord <- datos[[2]][ord]
    cumw <- cumsum(w_ord)/sum(w_ord)
    cuantiles_val <- sapply(cortes, function(p) {
      min(x_ord[cumw >= p])
    })
    cuantiles_val <- round(cuantiles_val, 4)
  }

  names(cuantiles_val) <- paste0("cuantiles_", main_varname)
  row.names(cuantiles_val) <- paste0(cortes*100, "%")
  return(as.data.frame(cuantiles_val))
}
