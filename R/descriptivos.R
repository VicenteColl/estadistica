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
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  # Determinar nombres originales
  original_names <- names(x)

  # --- Seleccion de variable(s) ---
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
    varnames <- names(x)[variable]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("El nombre de la variable no es v\u00e1lido")
    varnames <- variable
  } else {
    stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
  }

  # Subconjunto con las variables seleccionadas
  x_sel <- x[, varnames, drop = FALSE]

  # --- Manejo de pesos ---
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1)
      stop("Para el c\u00e1lculo ponderado solo puedes seleccionar una variable y unos pesos")

    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("El nombre de los pesos no es v\u00e1lido")
      pesos_name <- pesos
    } else if (is.numeric(pesos)) {
      if (pesos > ncol(x)) stop("Selecci\u00f3n err\i00f3nea de pesos")
      pesos_name <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    if (pesos_name == varnames)
      stop("No puedes usar la misma variable como dato y como peso")

    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_name]])
    varnames <- varnames[1]
  }

  # --- Comprobacion tipo de variable ---
  if (!all(sapply(x_sel, is.numeric))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }

  # --- Principales medidas ---
  if (is.null(pesos)) {
    valor_media <- media(x_sel) %>% t() %>% as.data.frame()
    valor_cuartiles <- cuantiles(x_sel, cortes = c(0, 0.25, 0.5, 0.75, 1))
    valor_varianza <- varianza(x_sel) %>% t() %>% as.data.frame()
    valor_desviacion <- desviacion(x_sel) %>% t() %>% as.data.frame()
    valor_coef_variacion <- coeficiente.variacion(x_sel) %>% t() %>% as.data.frame()
    ric <- cuantiles(x_sel, cortes = 0.75) - cuantiles(x_sel, cortes = 0.25)
    valor_forma <- medidas.forma(x_sel) %>% as.data.frame()
    valor_moda <- moda(x_sel)
  } else {
    valor_media <- media(x_sel, variable = 1, pesos = 2) %>% as.data.frame()
    valor_cuartiles <- cuantiles(x_sel, variable = 1, pesos = 2, cortes = c(0, 0.25, 0.5, 0.75, 1))
    valor_varianza <- varianza(x_sel, variable = 1, pesos = 2) %>% as.data.frame()
    valor_desviacion <- desviacion(x_sel, variable = 1, pesos = 2) %>% as.data.frame()
    valor_coef_variacion <- coeficiente.variacion(x_sel, variable = 1, pesos = 2) %>% as.data.frame()
    ric <- cuantiles(x_sel, variable = 1, pesos = 2, cortes = 0.75) -
      cuantiles(x_sel, variable = 1, pesos = 2, cortes = 0.25)
    valor_forma <- medidas.forma(x_sel, variable = 1, pesos = 2)
    valor_moda <- moda(x_sel, variable = 1, pesos = 2)
  }

  # --- Ensamblar resumen ---
  resumen <- data.table::rbindlist(
    list(
      valor_media,
      valor_cuartiles,
      valor_varianza,
      valor_desviacion,
      valor_coef_variacion,
      ric,
      valor_forma,
      valor_moda
    ),
    use.names = FALSE
  )

  resumen <- as.data.frame(resumen) %>% round(4)
  names(resumen) <- varnames

  num_modas <- nrow(valor_moda)
  row.names(resumen) <- c(
    "media", "mínimo", "cuartil 1", "mediana", "cuartil 3", "máximo",
    "varianza", "desviación típica", "coef.variación", "RIC",
    "asimetría", "curtosis", paste("moda_", 1:num_modas, sep = "")
  )

  # Mostrar resultados (si existe funci\u00f3#n auxiliar)
  if (exists(".mostrar_lista_resultados")) {
    .mostrar_lista_resultados(resumen, "Resumen de estadísticos descriptivos")
  } else {
    print(resumen)
  }

  # --- Exportar si se solicita ---
  if (exportar) {
    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")
    resumen_export <- cbind('Estadístico' = row.names(resumen), resumen)
    row.names(resumen_export) <- NULL

    openxlsx::writeData(wb, "Descriptivos", resumen_export)
    openxlsx::addStyle(
      wb, "Descriptivos",
      style = openxlsx::createStyle(numFmt = "0.0000"),
      rows = 2:(nrow(resumen_export) + 1),
      cols = 2:(ncol(resumen_export) + 1),
      gridExpand = TRUE
    )
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  invisible(resumen)
}


