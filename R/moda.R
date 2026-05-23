#' @title Moda.
#'
#' @description Calcula la moda.
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
#' @return Si \code{pesos = NULL}, devuelve la moda de todas la variables seleccionadas en un \code{data.frame}. En caso contrario, devuelve únicamente la moda de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' @import dplyr
#'
#' @export
moda <- function(data, variable = NULL, pesos = NULL) {

  # Si no es data.frame, convertir (vector simple)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }

  # Capturar argumentos
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)

  # Definir variable
  legacy_var <- FALSE
  varnames <- NULL
  vars_expr <- NULL

  if (quo_is_null(var_quo)) {
    # Por defecto: todas las columnas num\u00e9ricas, enteras, factor o l\uu00f3gicas
    vars_expr <- where(~ is.numeric(.x) || is.integer(.x) || is.factor(.x) || is.logical(.x))
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

  # Pesos
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
        error = function(e) stop("El argumento 'pesos' debe ser el nombre o \u00edndice de una columna.")
      )
      if (!peso_name %in% names(data)) stop("La columna de pesos no existe en los datos")
    }
  }

  # Regla para ponderar
  if (!is.null(peso_name)) {
    # Solo una variable permitida con pesos
    if (legacy_var) {
      if (length(varnames) != 1) stop("Para moda ponderada solo puedes seleccionar una variable")
    } else {
      sel_vars <- tryCatch(
        tidyselect::eval_select(vars_expr, data = data),
        error = function(e) stop("Selecci\u00f3n no v\u00e1lida de pesos")
      )
      if (length(sel_vars) != 1) stop("Para moda ponderada solo puedes seleccionar una variable")
    }
  }

  # Seleccionar columnas
  if (legacy_var) {
    selected_vars <- varnames
  } else {
    selected_vars <- names(tidyselect::eval_select(vars_expr, data = data))
  }

  # calcula la moda en un dataframe (con o sin pesos) ---
  compute_mode <- function(df, vars, peso_name = NULL) {
    if (is.null(peso_name)) {
      # Sin pesos: calcular moda para cada variable
      res <- list()
      for (v in vars) {
        modas_df <- .moda_int(df[[v]])
        modas_df$variable <- v
        res[[v]] <- modas_df
      }
      bind_rows(res)
    } else {
      # Con pesos: solo una variable
      v <- vars[1]
      modas_df <- .moda_pond_int(df[[v]], pesos = df[[peso_name]])
      modas_df$variable <- v
      modas_df
    }
  }

  # Calcula respetando grupos
  if (inherits(data, "grouped_df")) {
    # Datos agrupados: usar group_modify
    result <- data %>%
      dplyr::group_modify(~ {
        compute_mode(.x, vars = selected_vars, peso_name = peso_name)
      }) %>%
      dplyr::ungroup()
  } else {
    # Datos no agrupados
    result <- compute_mode(data, vars = selected_vars, peso_name = peso_name)
  }

  class(result) <- c("resumen", class(result))
  return(result)
}

