#' @title Media (aritmética).
#'
#' @description Calcula la media aritmética.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: style="width: 25\%;"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @param data Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la media (aritmética) de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la media de la variable para la que se ha facilitado la distribución de frecuencias.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' Si se obtiene la media (muestral) a partir de los datos brutos, como generalmente hacen los softwares:
#'
#' \deqn{\displaystyle \bar{x} = \frac{\sum_{i=1}^{n} x_i}{n}}
#'
#' Si se desea obtener la media (muestral) a partir de una tabla estadística se utiliza la expresión:
#'
#' \deqn{\displaystyle \bar{x} = \frac{\sum_{i=1}^{I} x_i \cdot n_i}{n}}
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la media poblacional:
#'
#' \deqn{\displaystyle \mu = \frac{\sum_{i=1}^{N} x_i}{N}}
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
#' media1 <- media(startup[1])
#' media2 <- media(startup,variable=1)
#' media3 <- media(salarios2018,variable=6,pesos=7)
#'
#' @importFrom stats na.omit weighted.mean
#' @import dplyr rlang tidyselect
#'
#' @export
media <- function(data, variable = NULL, pesos = NULL) {

  # convertir a dataframe si no lo es (para vectores simples)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }

  # Capturar argumentos
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)

  # Determinar si se accede directamente a 'variable' (indices o nombres) o tidy
  legacy_var <- FALSE
  varnames <- NULL
  vars_expr <- NULL

  if (quo_is_null(var_quo)) {
    vars_expr <- where(is.numeric)
    legacy_var <- FALSE
  } else {
    eval_res <- tryCatch(
      eval_tidy(var_quo, env = caller_env()),
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
        stop("Alguna variable seleccionada no existe en el dataframe")
      }
    } else {
      legacy_var <- FALSE
      vars_expr <- var_quo
    }
  }

  # Manejo de pesos
  peso_name <- NULL
  if (!quo_is_null(pesos_quo)) {
    pesos_eval <- tryCatch(
      eval_tidy(pesos_quo, env = caller_env()),
      error = function(e) NULL
    )

    if (is.numeric(pesos_eval) || is.character(pesos_eval)) {
      if (is.numeric(pesos_eval)) {
        peso_name <- names(data)[pesos_eval[1]]
      } else {
        peso_name <- pesos_eval[1]
      }
      if (!peso_name %in% names(data)) {
        stop("El nombre de los pesos no es v\u00e1lido")
      }
    } else {
      peso_name <- tryCatch(
        as_name(pesos_quo),
        error = function(e) {
          stop("El argumento 'pesos' debe ser el nombre de una variable o su \u00f3ndice")
        }
      )
      if (!peso_name %in% names(data)) {
        stop("La columna de pesos no existe en los datos")
      }
    }
  }

  # Calculo de medias (respeta grupos)
  if (is.null(peso_name)) {
    if (legacy_var) {
      result <- data %>%
        select(all_of(varnames)) %>%
        summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
    } else {
      result <- data %>%
        summarise(across({{ vars_expr }}, \(x) mean(x, na.rm = TRUE)))
    }
  } else {
    # Media ponderada (solo una variable)
    if (legacy_var) {
      if (length(varnames) != 1) {
        stop("Para el c\u00e1lculo ponderado solo puedes seleccionar una variable.")
      }
      var_name <- varnames[1]
    } else {
      sel_vars <- tryCatch(
        tidyselect::eval_select(vars_expr, data = data),
        error = function(e) stop("Selecci\u00f3n inv\u00e1lida para ponderaci\u00f3n")
      )
      if (length(sel_vars) != 1) {
        stop("Para el \u00e1lculo ponderado solo puedes seleccionar una variable.")
      }
      var_name <- names(sel_vars)[1]
    }

    if (var_name == peso_name) {
      stop("No puedes usar la misma variable como dato y como peso.")
    }

    result <- data %>%
      summarise(
        !!var_name := weighted.mean(.data[[var_name]], .data[[peso_name]], na.rm = TRUE)
      )
  }

  class(result) <- c("resumen", class(result))
  return(result)
}


