#' @title Mediana.
#'
#' @description Calcula la mediana.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{width = 200px}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la mediana de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la mediana de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' La mediana se obtiene a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{mediana.png}{width = 640px}}
#' \if{latex}{\figure{mediana.png}{options: scale=.8}}
#'
#' donde: Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' @seealso \code{\link{media}}, \code{\link{cuantiles}}
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' mediana1 <- mediana(startup[1])
#' mediana2 <- mediana(startup,variable=1)
#' mediana3 <- mediana(salarios2018,variable=6,pesos=7)
#'
#' @import dplyr
#'
#' @export
mediana <- function(data, variable = NULL, pesos = NULL) {
  
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }
  
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)
  
  # --- Procesar variable ---
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
        if (any(eval_res > ncol(data))) stop("Selecci\u00f3n err\u00f3nea de variables")
        varnames <- names(data)[eval_res]
      } else {
        if (!all(eval_res %in% names(data))) stop("Nombre de variable no v\u00e1lido")
        varnames <- eval_res
      }
    } else {
      legacy_var <- FALSE
      vars_expr <- var_quo
    }
  }
  
  # --- Procesar pesos ---
  peso_name <- NULL
  if (!quo_is_null(pesos_quo)) {
    pesos_eval <- tryCatch(
      eval_tidy(pesos_quo, env = caller_env()),
      error = function(e) NULL
    )
    if (is.numeric(pesos_eval) || is.character(pesos_eval)) {
      if (is.numeric(pesos_eval)) {
        if (pesos_eval > ncol(data)) stop("Selecci\u00f3n err\u00f3nea de pesos")
        peso_name <- names(data)[pesos_eval[1]]
      } else {
        if (!(pesos_eval[1] %in% names(data))) stop("Nombre de pesos no v\u00e1lido")
        peso_name <- pesos_eval[1]
      }
    } else {
      peso_name <- tryCatch(
        as_name(pesos_quo),
        error = function(e) stop("El argumento 'pesos' debe ser una columna única o su nombre/índice")
      )
      if (!peso_name %in% names(data)) stop("La columna de pesos no existe en los datos")
    }
  }
  
  # --- Cálculo de la mediana ---
  if (is.null(peso_name)) {
    if (legacy_var) {
      result <- data %>%
        select(all_of(varnames)) %>%
        summarise(across(everything(), ~ .mediana_int(.x, pesos = NULL)))
    } else {
      result <- data %>%
        summarise(across({{ vars_expr }}, ~ .mediana_int(.x, pesos = NULL)))
    }
  } else {
    if (legacy_var) {
      if (length(varnames) != 1) stop("Para mediana ponderada solo una variable")
      var_name <- varnames[1]
    } else {
      sel_vars <- tidyselect::eval_select(vars_expr, data = data)
      if (length(sel_vars) != 1) stop("Para mediana ponderada solo una variable")
      var_name <- names(sel_vars)[1]
    }
    if (var_name == peso_name) stop("La variable y los pesos no pueden ser la misma columna")
    
    result <- data %>%
      summarise(!!var_name := .mediana_int(.data[[var_name]], pesos = .data[[peso_name]]))
  }
  
  result <- result %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
  class(result) <- c("resumen", class(result))
  return(result)
}
