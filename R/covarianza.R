#' @title Covarianza.
#'
#' @description Calcula la covarianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: style="width: 25\%;"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @param data Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
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
#' \deqn{\displaystyle
#' S_{XY} =
#' \frac{\sum_{i=1}^{n}(x_i - \bar{x})(y_i - \bar{y})}{n}
#' }
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la covarianza a partir de la expresión:
#'
#' \deqn{\displaystyle
#' S_{{XY}}^* =
#' \frac{\sum_{i=1}^{n}(x_i - \bar{x})(y_i - \bar{y})}{n-1}
#' }
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' si los datos vienen dispuestos en una tabla de frecuencias se debe calcular una covarianza (o cuasicovarianza) muestral ponderada.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la covarianza poblacional:
#'
#' \deqn{\displaystyle
#' \sigma_{XY} =
#' \frac{\sum_{i=1}^{N}(x_i - \mu_X)(y_i - \mu_Y)}{N}
#' }

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
covarianza <- function(data,
                       variable = NULL,
                       pesos = NULL,
                       tipo = c("muestral", "cuasi")) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # Convertir si no es dataframe
  if (!is.data.frame(data)) {
    data <- data.frame(x = data)
  }

  # Capturar variable
  var_quo <- rlang::enquo(variable)
  pesos_quo <- rlang::enquo(pesos)

  # Seleccion de varaible

  legacy_var <- FALSE
  varnames <- NULL
  vars_expr <- NULL

  if (rlang::quo_is_null(var_quo)) {

    num_vars <- names(data)[sapply(data, is.numeric)]

    if (length(num_vars) < 2) {
      stop("Se necesitan al menos dos variables num\u00e9ricas.")
    }

    varnames <- num_vars[1:2]
    legacy_var <- TRUE

  } else {

    eval_res <- tryCatch(
      rlang::eval_tidy(var_quo, env = rlang::caller_env()),
      error = function(e) NULL
    )


    if (is.numeric(eval_res) || is.character(eval_res)) {

      legacy_var <- TRUE

      if (is.numeric(eval_res)) {
        varnames <- names(data)[eval_res]
      } else {
        varnames <- eval_res
      }

      if (!all(varnames %in% names(data))) {
        stop("Alguna variable seleccionada no existe en el dataframe.")
      }

    } else {

      # evaluacion tidy

      legacy_var <- FALSE
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

  # Pesos

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

      if (!peso_name %in% names(data)) {
        stop("El nombre de los pesos no es vl\u00e1ido.")
      }

    } else {

      peso_name <- tryCatch(
        rlang::as_name(pesos_quo),
        error = function(e) {
          stop("Los 'pesos' deben ser una sola columna.")
        }
      )

      if (!peso_name %in% names(data)) {
        stop("La columna de pesos no existe.")
      }
    }
  }


  var1 <- varnames[1]
  var2 <- varnames[2]

  if (is.null(peso_name)) {

    result <- data %>%
      dplyr::summarise(
        !!paste0(var1, "_", var2) := {

          x <- .data[[var1]]
          y <- .data[[var2]]

          ok <- stats::complete.cases(x, y)

          x <- x[ok]
          y <- y[ok]

          n_eff <- length(x)

          if (n_eff < 2) {
            NA_real_
          } else {

            cov_val <- stats::cov(x, y)

            if (tipo == "muestral") {
              cov_val <- cov_val * ((n_eff - 1) / n_eff)
            }

            round(cov_val, 4)
          }
        }
      )

  } else {

    if (peso_name %in% c(var1, var2)) {
      stop("La variable de pesos no puede coincidir con las variables seleccionadas.")
    }

    result <- data %>%
      dplyr::summarise(
        !!paste0(var1, "_", var2) := {

          x <- .data[[var1]]
          y <- .data[[var2]]
          w <- .data[[peso_name]]

          ok <- stats::complete.cases(x, y, w)

          x <- x[ok]
          y <- y[ok]
          w <- w[ok]

          n_eff <- length(x)

          if (n_eff < 2) {
            NA_real_
          } else {

            media_x <- sum(x * w) / sum(w)
            media_y <- sum(y * w) / sum(w)

            sum_cov <- sum((x - media_x) * (y - media_y) * w)

            if (tipo == "muestral") {
              cov_val <- sum_cov / sum(w)
            } else {
              cov_val <- sum_cov / (sum(w) - 1)
            }

            round(cov_val, 4)
          }
        }
      )
  }

  class(result) <- c("resumen", class(result))

  return(result)
}
