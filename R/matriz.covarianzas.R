#' @title Matriz de varianzas y covarianzas.
#'
#' @description Obtiene la matriz de varianzas y covarianzas.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @usage matriz.covar(x,
#'               variable = NULL,
#'               tipo = c("muestral","cuasi"),
#'               exportar = FALSE)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param tipo Es un carácter. Por defecto de calcula la matriz de varianzas y covarianzas muestrales (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la matriz de cuasi-varianzas y cuasi-covarianzas muestrales.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return La función devuelve la matriz de varianzas-covarianzas (muestrales, por defecto) de las variables seleccionadas en un \code{data.frame}.
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
#' @details
#'
#' (1) Se obtiene la matriz de varianzas y covarianzas muestrales:
#'
#' \if{html}{\figure{matrizvarcovmuestra.png}{options: width="50\%" alt="Figure: matrizvarcovmuestra.png"}}
#' \if{latex}{\figure{matrizvarcovmuestra.png}{options: width=8cm}}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) facilitan la matriz de cuasi-varianzas y cuasi-covarianzas muestrales:
#'
#' \if{html}{\figure{matrizvarcovcuasi.png}{options: width="55\%" alt="Figure: matrizvarcovcuasi.png"}}
#' \if{latex}{\figure{matrizvarcovcuasi.png}{options: width=8cm}}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la matriz de varianzas y covarianzas poblacional:
#'
#' \if{html}{\figure{matrizvarcovpob.png}{options: width="55\%" alt="Figure: matrizvarcovpob.png"}}
#' \if{latex}{\figure{matrizvarcovpob.png}{options: width=8cm}}
#'
#' @seealso \code{\link{varianza}}, \code{\link{desviacion}}
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
#' matriz_covarianzas1 <- matriz.covar(startup)
#' matriz_covarianzas2 <- matriz.covar(startup, tipo= "cuasi")
#'
#' @importFrom stats na.omit cov
#' @import dplyr
#'
#' @export
matriz.covar <- function(x,
                         variable = NULL,
                         tipo = c("muestral", "cuasi"),
                         exportar = FALSE) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # --- Configuracion temporal para evitar notacion cientifica ---
  old_options <- options()
  options(scipen = 999, digits = 15)
  on.exit(options(old_options), add = TRUE)

  # --- Normalizar entrada ---
  if (!is.data.frame(x)) {
    var_name <- deparse(substitute(x))
    if (is.vector(x)) {
      x <- data.frame(x)
      names(x) <- if (grepl("\\$", var_name)) sub(".*\\$", "", var_name) else "variable"
    } else {
      stop("El argumento x debe ser un data.frame o una lista de variables num\u00e9ricas.")
    }
  }

  varnames <- names(x)

  # --- Seleccion de variables ---
  if (is.null(variable)) {
    varcuan <- names(x)[sapply(x, is.numeric)]
    if (length(varcuan) == 0) stop("No hay variables num\u00e9ricas para calcular la matriz.")
    x <- x[, varcuan, drop = FALSE]
    varnames <- varcuan
  } else {
    if (is.numeric(variable)) {
      if (!all(variable <= ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables.")
      x <- x[, variable, drop = FALSE]
    } else if (is.character(variable)) {
      if (!all(variable %in% varnames)) stop("El nombre de alguna variable no es v\u00e1lido.")
      x <- x[, variable, drop = FALSE]
    } else {
      stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter.")
    }
    varnames <- names(x)
  }

  # --- Verificar tipo de variables ---
  if (!all(sapply(x, is.numeric))) {
    stop("No puede calcularse la matriz de varianzas-covarianzas: alguna variable no es cuantitativa.")
  }

  # --- Eliminar filas con NA en cualquiera de las variables ---
  x <- na.omit(x)
  n_eff <- nrow(x)
  if (n_eff < 2) stop("No hay suficientes observaciones completas para calcular la matriz.")

  # --- Preparar matriz vaca i---
  k <- ncol(x)
  matriz_covar <- matrix(NA, nrow = k, ncol = k)
  colnames(matriz_covar) <- varnames
  rownames(matriz_covar) <- varnames

  # --- Rellenar matriz con funciones personalizadas ---
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        matriz_covar[i, j] <- varianza(x[, i, drop = FALSE],
                                       variable = 1,
                                       tipo = tipo)
      } else {
        matriz_covar[i, j] <- covarianza(x[, c(i, j)],
                                         variable = 1:2,
                                         tipo = tipo)
      }
    }
  }

  # --- Asegurar simetria ---
  matriz_covar[lower.tri(matriz_covar)] <- t(matriz_covar)[lower.tri(matriz_covar)]
  matriz_covar <- round(matriz_covar, 4)

  # --- Exportar si se solicita ---
  if (exportar) {
    filename <- paste0("Matriz_de_covarianzas_",
                       format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                       ".xlsx")
    rio::export(matriz_covar, rowNames = TRUE, file = filename)
  }

  return(matriz_covar)
}
