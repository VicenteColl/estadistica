#' @title Covarianza.
#'
#' @description Calcula la covarianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @usage covarianza(x,
#' variable = NULL,
#' pesos = NULL,
#' tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x solo tiene 2 variables (columnas), el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la covarianza muestral (tipo = "muestral"). Si tipo = "cuasi", se calcula la cuasi-covarianza muestral.
#'
#' @return Esta función devuelve la covarianza en un objeto de la clase \code{vector}.
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
#' (1) La covarianza muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{covarianzamuestra.png}{options: width="50\%" alt="Figure: covarianzamuestra.png"}}
#' \if{latex}{\figure{covarianzamuestra.png}{options: width=6cm}}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la covarianza a partir de la expresión:
#'
#' \if{html}{\figure{covarianzacuasi.png}{options: width="50\%" alt="Figure: covarianzacuasi.png"}}
#' \if{latex}{\figure{covarianzacuasi.png}{options: width=6cm}}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la covarianza poblacional:
#'
#' \if{html}{\figure{covarianzapob.png}{options: width="50\%" alt="Figure: covarianzapob.png"}}
#' \if{latex}{\figure{covarianzapob.png}{options: width=6cm}}

#' @seealso \code{\link{varianza}}, \code{\link{desviacion}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom stats cov
#' @import dplyr
#'
#' @export
covarianza <- function(x,
                       variable = NULL,
                       pesos = NULL,
                       tipo = c("muestral", "cuasi")) {

  # --- Validacion de tipo ---
  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # --- Normalizacion del input ---
  if (!is.data.frame(x)) {
    var_name <- deparse(substitute(x))
    if (is.vector(x)) {
      x <- data.frame(x)
      names(x) <- if (grepl("\\$", var_name)) sub(".*\\$", "", var_name) else "variable"
    } else {
      stop("El argumento 'x' debe ser un data.frame o un vector num\u00e9rico.")
    }
  }

  # --- Guardar nombres originales ---
  varnames <- names(x)

  # --- Seleccion de variables ---
  if (is.null(variable)) {
    if (ncol(x) != 2) {
      stop("Debes seleccionar exactamente dos variables para calcular la covarianza.")
    }
  } else {
    if (is.numeric(variable)) {
      if (!all(variable <= ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables.")
      x <- x[, variable, drop = FALSE]
    } else if (is.character(variable)) {
      if (!all(variable %in% varnames)) stop("Nombre de variable no v\u00e1lido.")
      x <- x[, variable, drop = FALSE]
    } else {
      stop("El argumento 'variable' debe ser numérico o de tipo car\u00e1cter.")
    }
  }

  # --- Nombres tras subsetting ---
  varnames <- names(x)
  if (length(varnames) != 2) stop("Debes proporcionar exactamente dos variables cuantitativas.")

  # --- Comprobación de tipos ---
  if (!all(sapply(x, is.numeric))) {
    stop("No puede calcularse la covarianza: alguna variable seleccionada no es cuantitativa.")
  }

  # --- Caso SIN pesos ---
  if (is.null(pesos)) {
    # Eliminar observaciones incompletas
    x <- na.omit(x)
    n_eff <- nrow(x)
    if (n_eff < 2) stop("No hay suficientes observaciones completas para calcular la covarianza.")

    # Calcular factor muestral correctamente
    factor <- if (tipo == "muestral") (n_eff - 1) / n_eff else 1

    # Calcular covarianza
    cov_val <- factor * cov(x[[1]], x[[2]], use = "complete.obs")

    result <- round(as.numeric(cov_val), 4)
    names(result) <- paste0("covarianza_", varnames[1], "_", varnames[2])
    return(result)
  }

  # --- Caso CON pesos ---
  if (length(pesos) != 1) {
    stop("Solo se admite una variable de pesos.")
  }

  if (is.character(pesos)) {
    if (!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido.")
    pesos_idx <- match(pesos, names(x))
  } else if (is.numeric(pesos)) {
    pesos_idx <- pesos
  } else {
    stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter.")
  }

  # Reconstruir data.frame con pesos
  x <- x[, c(1:2, pesos_idx), drop = FALSE]
  names(x) <- c("var1", "var2", "pesos")

  # Eliminar NA
  x <- na.omit(x)
  n_eff <- nrow(x)
  if (n_eff < 2) stop("No hay suficientes observaciones completas para calcular la covarianza ponderada.")

  # Calcular medias ponderadas
  media1 <- sum(x$var1 * x$pesos, na.rm = TRUE) / sum(x$pesos, na.rm = TRUE)
  media2 <- sum(x$var2 * x$pesos, na.rm = TRUE) / sum(x$pesos, na.rm = TRUE)

  # Covarianza ponderada
  sum_cuad <- sum((x$var1 - media1) * (x$var2 - media2) * x$pesos, na.rm = TRUE)

  if (tipo == "muestral") {
    cov_val <- sum_cuad / sum(x$pesos, na.rm = TRUE)
  } else {
    cov_val <- sum_cuad / (sum(x$pesos, na.rm = TRUE) - 1)
  }

  result <- round(as.numeric(cov_val), 4)
  names(result) <- paste0("covarianza_", varnames[1], "_", varnames[2])

  return(result)
}
