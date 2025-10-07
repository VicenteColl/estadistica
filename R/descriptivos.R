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
  options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)

  if (!is.data.frame(x)) x <- as.data.frame(x)

  # Selección de variables
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no válido")
    varnames <- variable
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selección errónea de variables")
    varnames <- names(x)[variable]
  } else stop("El argumento 'variable' debe ser numérico o carácter")

  x_sel <- x[, varnames, drop = FALSE]

  # Manejo de pesos
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1)
      stop("Para cálculo ponderado solo una variable y un peso")
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

  if (!all(sapply(x_sel, is.numeric))) stop("Alguna variable seleccionada no es cuantitativa")

  # Lista para almacenar resultados por variable
  lista_res <- list()

  for (v in varnames) {
    y <- x_sel[[v]]

    media_val <- media(y)
    var_val <- varianza(y)
    desv_val <- desviacion(y)
    coef_val <- coeficiente.variacion(y)
    cuan <- cuantiles(y, cortes = c(0,0.25,0.5,0.75,1))
    ric_val <- cuan[4,1] - cuan[2,1]  # cuartil3 - cuartil1
    forma_val <- medidas.forma(y)
    moda_val <- moda(y)

    # Combinar en vector nombrado
    vec <- c(
      media = media_val,
      minimo = cuan[1,1],
      cuartil1 = cuan[2,1],
      mediana = cuan[3,1],
      cuartil3 = cuan[4,1],
      maximo = cuan[5,1],
      varianza = var_val,
      desviacion_tipica = desv_val,
      coef_variacion = coef_val,
      RIC = ric_val,
      asimetria = forma_val[1],
      curtosis = forma_val[2]
    )

    # Añadir modas
    if (!is.null(moda_val)) {
      names(moda_val) <- paste0("moda_", seq_along(moda_val))
      vec <- c(vec, moda_val)
    }

    lista_res[[v]] <- vec
  }

  resumen <- do.call(cbind, lista_res)
  resumen <- round(resumen, 4)

  # Exportar si se solicita
  if (exportar) {
    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")
    resumen_export <- cbind('Estadístico' = rownames(resumen), resumen)
    rownames(resumen_export) <- NULL
    openxlsx::writeData(wb, "Descriptivos", resumen_export)
    openxlsx::addStyle(
      wb, "Descriptivos",
      style = openxlsx::createStyle(numFmt = "0.0000"),
      rows = 2:(nrow(resumen_export)+1),
      cols = 2:(ncol(resumen_export)+1),
      gridExpand = TRUE
    )
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(resumen)
}
