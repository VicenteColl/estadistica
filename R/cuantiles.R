#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcuantiles.png}{options: style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcuantiles.png}{options: width=3cm}}
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
#' \if{html}{\figure{cuantiles.png}{options:style="width: 85\%;" alt="Figure: cuantiles.png"}}
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
#' @importFrom stats quantile
#'
#' @export
cuantiles <- function(x, variable = NULL, pesos = NULL,
                      cortes = c(0.25, 0.5, 0.75),
                      exportar = FALSE) {

  # Asegurar data.frame
  if (!is.data.frame(x)) x <- as.data.frame(x)

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

  # --- Manejo de pesos ---
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1)
      stop("Para calcular cuantiles ponderados solo puedes seleccionar una variable y un peso")

    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("Nombre de pesos no vl\u00e1ido")
      pesos_name <- pesos
    } else if (is.numeric(pesos)) {
      if (any(pesos > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de pesos")
      pesos_name <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    if (pesos_name == varnames)
      stop("No puedes usar la misma variable como dato y como peso")

    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_name]])
    varnames <- varnames[1]
  }

  cortes <- sort(cortes)

  # --- Calculo de cuantiles usando tu funcion interna ---
  if (is.null(pesos)) {
    cuantiles_mat <- apply(x_sel, 2, .cuantiles.int, cortes = cortes)
  } else {
    cuantiles_mat <- .cuantiles.int(x_sel$variable, pesos = x_sel$pesos, cortes = cortes)
  }

  # Asegurar que es data.frame y poner nombres
  cuantiles_df <- as.data.frame(cuantiles_mat)
  rownames(cuantiles_df) <- paste0(cortes*100, "%")
  names(cuantiles_df) <- paste0("cuantiles_", varnames)

  # --- Exportar a Excel ---
  if (exportar) {
    filename <- paste0("Cuantiles_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Cuantiles")
    resumen_export <- cbind(Cuantil = rownames(cuantiles_df), cuantiles_df)
    rownames(resumen_export) <- NULL
    openxlsx::writeData(wb, "Cuantiles", resumen_export)
    openxlsx::addStyle(wb, "Cuantiles",
                       style = openxlsx::createStyle(numFmt = "0.0000"),
                       rows = 2:(nrow(resumen_export)+1),
                       cols = 2:(ncol(resumen_export)+1),
                       gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  class(cuantiles_df) <- c("resumen", class(cuantiles_df))
  return(cuantiles_df)
}
