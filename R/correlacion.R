#' @title Coeficiente de correlación.
#'
#' @description Calcula el coeficiente de correlación de Pearson.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcorrelacion.png}{width = 200px}}
#' \if{latex}{\figure{qrcorrelacion.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Esta función devuelve el valor del coeficiente de correlación lineal en un objeto de la clase \code{vector}.
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
#' El coeficiente de correlación muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{correlacion.png}{width = 400px}}
#' \if{latex}{\figure{correlacion.png}{options: width=5.5cm}}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \if{html}{\figure{correlacionpob.png}{width = 240px}}
#' \if{latex}{\figure{correlacionpob.png}{options: width=3.5cm}}
#'
#' @seealso \code{\link{matriz.correlacion}}, \code{\link{covarianza}},\code{\link{matriz.covar}}
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
#' correlacion1 <- correlacion(startup[,c(1,3)])
#' correlacion2 <- correlacion(startup,variable=c(1,3))
#'
#' @importFrom stats cor
#' @import dplyr rlang
#'
#' @export
correlacion <- function(data,
                        variable = NULL,
                        pesos = NULL) {

  # =========================================================
  # Validación inicial
  # =========================================================

  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data.frame.")
  }

  # =========================================================
  # Captura tidy evaluation
  # =========================================================

  var_quo <- rlang::enquo(variable)
  pesos_quo <- rlang::enquo(pesos)

  # =========================================================
  # Selección de variables
  # =========================================================

  legacy_var <- FALSE
  varnames <- NULL
  vars_expr <- NULL

  if (rlang::quo_is_null(var_quo)) {

    num_vars <- names(data)[sapply(data, is.numeric)]

    if (length(num_vars) < 2) {
      stop("Se necesitan al menos dos variables numéricas.")
    }

    varnames <- num_vars[1:2]
    legacy_var <- TRUE

  } else {

    eval_res <- tryCatch(
      rlang::eval_tidy(var_quo, env = rlang::caller_env()),
      error = function(e) NULL
    )

    # -------------------------------------------------------
    # Compatibilidad legacy
    # -------------------------------------------------------

    if (is.numeric(eval_res) || is.character(eval_res)) {

      legacy_var <- TRUE

      if (is.numeric(eval_res)) {
        varnames <- names(data)[eval_res]
      } else {
        varnames <- eval_res
      }

      if (!all(varnames %in% names(data))) {
        stop("Alguna variable seleccionada no existe.")
      }

    } else {

      # -----------------------------------------------------
      # Tidy evaluation
      # -----------------------------------------------------

      vars_expr <- var_quo

      sel_vars <- tidyselect::eval_select(
        expr = vars_expr,
        data = data
      )

      if (length(sel_vars) != 2) {
        stop("Debes seleccionar exactamente dos variables.")
      }

      varnames <- names(sel_vars)
    }
  }

  # =========================================================
  # Validaciones
  # =========================================================

  var1 <- varnames[1]
  var2 <- varnames[2]

  if (!is.numeric(data[[var1]]) ||
      !is.numeric(data[[var2]])) {
    stop("Las variables deben ser numéricas.")
  }

  # =========================================================
  # Pesos
  # =========================================================

  peso_name <- NULL

  if (!rlang::quo_is_null(pesos_quo)) {

    pesos_eval <- tryCatch(
      rlang::eval_tidy(pesos_quo, env = rlang::caller_env()),
      error = function(e) NULL
    )

    if (is.numeric(pesos_eval) || is.character(pesos_eval)) {

      if (is.numeric(pesos_eval)) {
        peso_name <- names(data)[pesos_eval[1]]
      } else {
        peso_name <- pesos_eval[1]
      }

    } else {

      peso_name <- tryCatch(
        rlang::as_name(pesos_quo),
        error = function(e) {
          stop("Selección de pesos inválida.")
        }
      )
    }

    if (!peso_name %in% names(data)) {
      stop("La variable de pesos no existe.")
    }

    if (!is.numeric(data[[peso_name]])) {
      stop("'pesos' debe ser numérica.")
    }
  }

  # =========================================================
  # Nombre resultado
  # =========================================================

  result_name <- paste0(var1, "_", var2)

  # =========================================================
  # SIN PESOS
  # =========================================================

  if (is.null(peso_name)) {

    result <- data %>%
      dplyr::summarise(
        !!result_name := {

          x <- .data[[var1]]
          y <- .data[[var2]]

          ok <- stats::complete.cases(x, y)

          x <- x[ok]
          y <- y[ok]

          if (length(x) < 2) {
            NA_real_
          } else {
            stats::cor(x, y)
          }
        }
      )

  } else {

    # =======================================================
    # CON PESOS
    # =======================================================

    result <- data %>%
      dplyr::summarise(
        !!result_name := {

          x <- .data[[var1]]
          y <- .data[[var2]]
          w <- .data[[peso_name]]

          ok <- stats::complete.cases(x, y, w)

          x <- x[ok]
          y <- y[ok]
          w <- w[ok]

          if (length(x) < 2) {
            NA_real_
          } else {

            media_x <- sum(x * w) / sum(w)
            media_y <- sum(y * w) / sum(w)

            dx <- x - media_x
            dy <- y - media_y

            sum_cross <- sum(dx * dy * w)
            sum_sq_x  <- sum(dx^2 * w)
            sum_sq_y  <- sum(dy^2 * w)

            sum_cross / sqrt(sum_sq_x * sum_sq_y)
          }
        }
      )
  }

  class(result) <- c("resumen", class(result))

  return(result)
}
