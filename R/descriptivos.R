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
  old_options <- options()
  on.exit(options(old_options), add = TRUE)
  options(scipen = 999)  # evita notación científica al imprimir

  if (!is.data.frame(x)) x <- as.data.frame(x)

  # Selección de variables
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("El nombre de variable no es válido")
    varnames <- variable
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selección errónea de variables")
    varnames <- names(x)[variable]
  } else stop("El argumento 'variable' debe ser numérico o carácter")

  x_sel <- x[, varnames, drop = FALSE]

  # Manejo de pesos
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1) stop("Solo una variable y un peso para ponderado")
    pesos_col <- if (is.character(pesos)) pesos else names(x)[pesos]
    if (pesos_col == varnames) stop("No puedes usar la misma variable como dato y peso")
    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_col]])
    varnames <- varnames[1]
  }

  if (!all(sapply(x_sel, is.numeric))) stop("Alguna variable seleccionada no es cuantitativa")

  # --- Función para redondear ---
  redondear <- function(v) round(v, 4)

  # --- Cálculos ---
  medias <- redondear(sapply(x_sel, mean, na.rm = TRUE))
  minimos <- redondear(sapply(x_sel, min, na.rm = TRUE))
  cuartiles <- t(sapply(x_sel, function(v) quantile(v, probs = c(0.25,0.5,0.75), na.rm = TRUE)))
  maximos <- redondear(sapply(x_sel, max, na.rm = TRUE))
  varianzas <- redondear(sapply(x_sel, var, na.rm = TRUE))
  desviaciones <- redondear(sapply(x_sel, sd, na.rm = TRUE))
  coef_var <- redondear(desviaciones / medias)
  ric <- redondear(cuartiles[,3] - cuartiles[,1])
  asimetria <- redondear(sapply(x_sel, function(v) e1071::skewness(v, na.rm = TRUE)))
  curtosis <- redondear(sapply(x_sel, function(v) e1071::kurtosis(v, na.rm = TRUE)))

  # Modas
  modas_list <- lapply(x_sel, function(v) {
    t <- table(na.omit(v))
    valores <- as.numeric(names(t)[t == max(t)])
    valores
  })
  max_modas <- max(sapply(modas_list, length))
  modas_mat <- sapply(modas_list, function(v) { length(v) <- max_modas; v })
  modas_mat <- as.data.frame(modas_mat)
  names(modas_mat) <- varnames
  rownames(modas_mat) <- paste0("moda_", seq_len(max_modas))

  # Ensamblar
  resumen <- rbind(
    media = medias,
    minimo = minimos,
    cuartil1 = cuartiles[,1],
    mediana = cuartiles[,2],
    cuartil3 = cuartiles[,3],
    maximo = maximos,
    varianza = varianzas,
    desviacion_tipica = desviaciones,
    coef_variacion = coef_var,
    RIC = ric,
    asimetria = asimetria,
    curtosis = curtosis,
    modas_mat
  )
  resumen <- as.data.frame(resumen)

  # Exportar si se solicita
  if (exportar) {
    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")
    resumen_export <- cbind(Estadistico = row.names(resumen), resumen)
    row.names(resumen_export) <- NULL
    openxlsx::writeData(wb, "Descriptivos", resumen_export)
    openxlsx::addStyle(wb, "Descriptivos",
                       style = openxlsx::createStyle(numFmt = "0.0000"),
                       rows = 2:(nrow(resumen_export)+1),
                       cols = 2:(ncol(resumen_export)+1),
                       gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  # Clase especial para impresión
  class(resumen) <- c("resumen_descriptivos", class(resumen))

  return(resumen)
}

