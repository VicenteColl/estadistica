#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdescriptivos.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdescriptivos.png}{options: width=3cm}}
#'
#' @usage resumen.descriptivos(x,
#'                             variable = NULL,
#'                             pesos = NULL,
#'                             exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Esta función devuelve los principales estadísticos descriptivos muestrales en un objeto de tipo \code{data.frame}. Los descriptivos que se obtienen son: media, mínimo, cuartil 1, mediana, cuartil 3, máximo, varianza muestral, desviación típica muestral, coeficiente de variación, recorrido inter-cuartílico, asimetría, curtosis y moda.
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' descriptivos <- resumen.descriptivos(startup)
#'
#' @import dplyr openxlsx
#'
#' @export
resumen.descriptivos <- function(x, variable = NULL, pesos = NULL, exportar = FALSE) {
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # Seleccion de variables
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("El nombre de variable no es valido")
    varnames <- variable
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Seleccion erronea de variables")
    varnames <- names(x)[variable]
  } else stop("El argumento 'variable' debe ser numerico o caracter")

  x_sel <- x[, varnames, drop = FALSE]

  # Calculos
  valor_media <- media(x_sel, pesos = pesos)
  valor_varianza <- varianza(x_sel, pesos = pesos)
  valor_desviacion <- desviacion(x_sel, pesos = pesos)
  valor_coef <- coeficiente.variacion(x_sel, pesos = pesos)
  valor_cuantiles <- cuantiles(x_sel, pesos = pesos)
  valor_forma <- medidas.forma(x_sel, pesos = pesos)
  # Moda sin avisos
  valor_moda <- suppressWarnings(moda(x_sel, pesos = pesos))

  # Convertir todo a data.frames si no lo son
  valor_media <- as.data.frame(valor_media)
  valor_varianza <- as.data.frame(valor_varianza)
  valor_desviacion <- as.data.frame(valor_desviacion)
  valor_coef <- as.data.frame(valor_coef)
  valor_cuantiles <- as.data.frame(valor_cuantiles)
  valor_forma <- as.data.frame(valor_forma)
  valor_moda <- as.data.frame(valor_moda)

  # Ajustar filas de moda: si tiene menos de 1 fila, rellenar NA
  max_moda_filas <- nrow(valor_moda)
  if (max_moda_filas < 1) {
    valor_moda[1, ] <- NA
  }
  rownames(valor_moda) <- paste0("moda_", seq_len(nrow(valor_moda)))

  # Normalizar nombres de columnas
  colnames(valor_media) <- varnames
  colnames(valor_varianza) <- varnames
  colnames(valor_desviacion) <- varnames
  colnames(valor_coef) <- varnames
  colnames(valor_cuantiles) <- varnames
  colnames(valor_forma) <- varnames
  colnames(valor_moda) <- varnames

  # Asignar nombres de fila
  rownames(valor_media) <- "media"
  rownames(valor_varianza) <- "varianza"
  rownames(valor_desviacion) <- "desviacion"
  rownames(valor_coef) <- "coef_variacion"
  if (nrow(valor_cuantiles) == 3) rownames(valor_cuantiles) <- c("cuartil1","mediana","cuartil3")
  if (nrow(valor_forma) == 2) rownames(valor_forma) <- c("asimetria","curtosis")

  # Ensamblar
  resumen <- rbind(valor_media, valor_varianza, valor_desviacion,
                   valor_coef, valor_cuantiles, valor_forma, valor_moda)

  resumen <- round(resumen, 4)

  # Exportar
  if (exportar) {
    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")
    resumen_export <- cbind('Estadistico' = rownames(resumen), resumen)
    rownames(resumen_export) <- NULL
    openxlsx::writeData(wb, "Descriptivos", resumen_export)
    openxlsx::addStyle(wb, "Descriptivos",
                       style = openxlsx::createStyle(numFmt = "0.0000"),
                       rows = 2:(nrow(resumen_export)+1),
                       cols = 2:(ncol(resumen_export)+1),
                       gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  class(resumen) <- c("resumen", class(resumen))
  return(resumen)
}
