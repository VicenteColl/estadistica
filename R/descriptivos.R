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

  # --- Opciones para evitar notación científica ---
  old_options <- options()
  options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)

  # --- Asegurar data.frame ---
  if (!is.data.frame(x)) x <- as.data.frame(x)

  # --- Selección de variables ---
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.character(variable)) {
    if (!all(variable %in% names(x))) stop("Nombre de variable no válido")
    varnames <- variable
  } else if (is.numeric(variable)) {
    if (any(variable > ncol(x))) stop("Selección errónea de variables")
    varnames <- names(x)[variable]
  } else stop("El argumento 'variable' debe ser numérico o carácter")

  # --- Subconjunto ---
  x_sel <- x[, varnames, drop = FALSE]

  # --- Manejo de pesos ---
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1)
      stop("Para cálculo ponderado solo una variable y un peso")

    if (is.character(pesos)) {
      if (!pesos %in% names(x)) stop("Nombre de pesos no válido")
      pesos_name <- pesos
    } else if (is.numeric(pesos)) {
      pesos_name <- names(x)[pesos]
    }

    if (pesos_name == varnames) stop("No puedes usar la misma variable como dato y peso")
    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_name]])
    varnames <- varnames[1]
  }

  # --- Comprobación numérica ---
  if (!all(sapply(x_sel, is.numeric))) stop("Alguna variable seleccionada no es cuantitativa")

  # --- Función auxiliar para medidas por variable ---
  calcular_medidas <- function(vec, pesos = NULL) {
    df <- data.frame()

    # Media
    df["media", 1] <- if (is.null(pesos)) mean(vec, na.rm = TRUE) else sum(vec * pesos) / sum(pesos)

    # Cuantiles
    qs <- c(0, 0.25, 0.5, 0.75, 1)
    df[c("mínimo", "cuartil 1", "mediana", "cuartil 3", "máximo"), 1] <-
      as.numeric(quantile(vec, probs = qs, na.rm = TRUE, names = FALSE))

    # Varianza
    n_eff <- sum(!is.na(vec))
    factor <- if (is.null(pesos)) (n_eff - 1)/n_eff else 1
    var_val <- if (is.null(pesos)) var(vec, na.rm = TRUE) * factor else sum(pesos * (vec - mean(vec * pesos/sum(pesos)))^2)/sum(pesos)
    df["varianza", 1] <- var_val

    # Desviación típica
    df["desviación típica", 1] <- sqrt(var_val)

    # Coeficiente de variación
    df["coef.variación", 1] <- df["desviación típica", 1] / df["media", 1]

    # Rango intercuartílico
    df["RIC", 1] <- df["cuartil 3", 1] - df["cuartil 1", 1]

    # Medidas de forma
    desv <- df["desviación típica", 1]
    momento3 <- mean((vec - df["media", 1])^3, na.rm = TRUE)
    momento4 <- mean((vec - df["media", 1])^4, na.rm = TRUE)
    df["asimetría", 1] <- momento3 / (desv^3)
    df["curtosis", 1] <- momento4 / (desv^4) - 3

    # Moda
    modas <- names(sort(table(vec), decreasing = TRUE))[1]
    for (i in seq_along(modas)) {
      df[paste0("moda_", i), 1] <- as.numeric(modas[i])
    }

    return(df)
  }

  # --- Calcular medidas para cada variable ---
  resultados <- lapply(varnames, function(v) {
    if (!is.null(pesos)) {
      calcular_medidas(x_sel$variable, x_sel$pesos)
    } else {
      calcular_medidas(x_sel[[v]])
    }
  })

  # --- Combinar en un solo data.frame ---
  resumen <- do.call(cbind, resultados)
  colnames(resumen) <- varnames
  resumen <- round(resumen, 4)

  # --- Exportar a Excel ---
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

  return(resumen)
}
