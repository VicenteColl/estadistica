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
resumen.descriptivos <- function(x,
                                 variable = NULL,
                                 pesos = NULL,
                                 exportar = FALSE) {

  # Guardar opciones originales
  old_options <- options()
  options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)


  # Asegurar data.frame
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

  # Subconjunto con las variables seleccionadas
  x_sel <- x[, varnames, drop = FALSE]

  # Manejo de pesos
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1) stop("Para cálculo ponderado solo una variable y un peso")
    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("Nombre de pesos no válido")
      pesos_col <- pesos
    } else if (is.numeric(pesos)) {
      pesos_col <- names(x)[pesos]
    }
    if (pesos_col == varnames) stop("No puedes usar la misma variable como dato y peso")
    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_col]])
    varnames <- varnames[1]
  }

  # Verificación de tipo numérico
  if (!all(sapply(x_sel, is.numeric))) stop("Alguna variable seleccionada no es cuantitativa")

  # --- Función auxiliar para formatear números ---
  format_num <- function(x) {
    x <- round(x, 4)
    x
  }

  # --- Cálculo de medidas ---
  medias <- sapply(x_sel, function(v) format_num(mean(v, na.rm = TRUE)))
  minimos <- sapply(x_sel, function(v) format_num(min(v, na.rm = TRUE)))
  cuartiles <- t(sapply(x_sel, function(v) format_num(quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))))
  maximos <- sapply(x_sel, function(v) format_num(max(v, na.rm = TRUE)))
  varianzas <- sapply(x_sel, function(v) format_num(var(v, na.rm = TRUE)))
  desviaciones <- sapply(x_sel, function(v) format_num(sd(v, na.rm = TRUE)))
  coef_var <- format_num(desviaciones / medias)
  ric <- format_num(cuartiles[,3] - cuartiles[,1])

  # Medidas de forma
  asimetria <- sapply(x_sel, function(v) format_num(e1071::skewness(v, na.rm = TRUE)))
  curtosis <- sapply(x_sel, function(v) format_num(e1071::kurtosis(v, na.rm = TRUE)))

  # Modas (devuelve lista para cada variable, con relleno NA)
  modas_list <- lapply(x_sel, function(v) {
    v_clean <- na.omit(v)
    t <- table(v_clean)
    valores <- names(t)[t == max(t)]
    valores_num <- as.numeric(valores)
    valores_num
  })
  max_modas <- max(sapply(modas_list, length))
  modas_mat <- sapply(modas_list, function(v) {
    length(v) <- max_modas
    v
  })
  modas_mat <- as.data.frame(modas_mat)
  names(modas_mat) <- varnames
  rownames(modas_mat) <- paste0("moda_", seq_len(max_modas))

  # Construir dataframe final
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

  resumen <- round(as.data.frame(resumen), 4)

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
                       rows = 2:(nrow(resumen_export) + 1),
                       cols = 2:(ncol(resumen_export) + 1),
                       gridExpand = TRUE)
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(resumen)
}

