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

  # Guardar opciones originales para evitar notación científica
  old_options <- options()
  options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)

  # Asegurar que x sea data.frame
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # Determinar variables seleccionadas
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selección errónea de variables")
    varnames <- names(x)[variable]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no válido")
    varnames <- variable
  } else stop("El argumento 'variable' debe ser numérico o carácter")

  # Subconjunto de variables seleccionadas
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

  # Verificación numérica
  if (!all(sapply(x_sel, is.numeric))) stop("Alguna variable seleccionada no es cuantitativa")

  # Inicializar lista para cada medida
  lista_medidas <- list()

  # Recorrer cada variable individualmente
  for (var in seq_along(varnames)) {
    col_data <- x_sel[, var, drop = FALSE]

    # Media
    lista_medidas[[paste0("media_", varnames[var])]] <- media(col_data)

    # Cuantiles
    cuant <- tryCatch({
      cuantiles(col_data, cortes = c(0,0.25,0.5,0.75,1))
    }, error = function(e) {
      data.frame(matrix(NA, nrow=5, ncol=1))
    })
    rownames(cuant) <- c("mínimo","cuartil 1","mediana","cuartil 3","máximo")
    colnames(cuant) <- paste0("cuantiles_", varnames[var])
    lista_medidas[[paste0("cuantiles_", varnames[var])]] <- cuant

    # Varianza
    lista_medidas[[paste0("varianza_", varnames[var])]] <- varianza(col_data)

    # Desviación típica
    lista_medidas[[paste0("desviacion_", varnames[var])]] <- desviacion(col_data)

    # Coeficiente de variación
    lista_medidas[[paste0("coef_variacion_", varnames[var])]] <- coeficiente.variacion(col_data)

    # RIC
    q75 <- tryCatch(cuantiles(col_data, cortes = 0.75), error=function(e) NA)
    q25 <- tryCatch(cuantiles(col_data, cortes = 0.25), error=function(e) NA)
    ric_val <- q75 - q25
    lista_medidas[[paste0("RIC_", varnames[var])]] <- ric_val

    # Medidas de forma
    forma <- tryCatch(medidas.forma(col_data), error=function(e) data.frame(asimetria=NA, curtosis=NA))
    lista_medidas[[paste0("forma_", varnames[var])]] <- forma

    # Moda
    mod <- tryCatch(moda(col_data), error=function(e) NA)
    lista_medidas[[paste0("moda_", varnames[var])]] <- mod
  }

  # Combinar todas las medidas en un data.frame
  resumen <- do.call(cbind, lapply(lista_medidas, function(x) {
    if (is.data.frame(x) || is.matrix(x)) {
      x
    } else {
      as.data.frame(t(x))
    }
  }))

  # Redondear
  resumen <- round(resumen, 4)

  # Exportar si se solicita
  if (exportar) {
    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")
    resumen_export <- cbind('Estadístico' = row.names(resumen), resumen)
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
