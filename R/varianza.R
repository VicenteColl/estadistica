#' @title Varianza.
#'
#' @description Calcula la varianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{width = 200px}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la varianza muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasivarianza muestral.
#'
#' @return Esta función devuelve un objeto de la clase \code{vector}. Si \code{tipo="muestral"}, devuelve la varianza muestral. Si \code{tipo="cuasi"}, devuelve la cuasi-varianza muestral.
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
#' (1) La expresión de la varianza muestral es:
#'
#' \if{html}{\figure{varianza.png}{width = 320px}}
#' \if{latex}{\figure{varianza.png}{options: width=5cm}}
#'
#' La varianza muestral así definida es el estimador máximo verosímil de la varianza de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasivarianza.png}{width = 320px}}
#' \if{latex}{\figure{cuasivarianza.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-varianza muestral y es un estimador insesgado de la varianza poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la varianza poblacional:
#'
#'
#' \if{html}{\figure{varianzapob.png}{width = 320px}}
#' \if{latex}{\figure{varianzapob.png}{options: width=5cm}}
#'
#' @seealso \code{\link{media}}, \code{\link{desviacion}}, \code{\link{coeficiente.variacion}}
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
#' varianza1 <- varianza(startup[1])
#' varianza2 <- varianza(startup,variable=1)
#' varianza3 <- varianza(startup,variable=1, tipo="cuasi")
#'
#' @importFrom stats var
#'
#' @export
varianza <- function(data, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {
  
  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))
  
  # Si no es data.frame, convertir (caso vector)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }
  # No convertir a tibble para preservar grupos
  
  # Capturar argumentos
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)
  
  # --- Procesar 'variable' (legacy vs tidy) ---
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
  
  # --- Procesar 'pesos' ---
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
        if (!(pesos_eval[1] %in% names(data))) stop("El nombre de los pesos no es v\u00e1lido")
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
  
  # --- Funciones auxiliares para la varianza ---
  var_no_pond <- function(x, tipo_var) {
    n_eff <- sum(!is.na(x))
    if (n_eff < 2) return(NA_real_)
    v <- stats::var(x, na.rm = TRUE)
    if (tipo_var == "muestral") {
      v * (n_eff - 1) / n_eff
    } else {
      v
    }
  }
  
  var_pond <- function(valor, peso, tipo_var) {
    ok <- !is.na(valor) & !is.na(peso)
    valor <- valor[ok]
    peso <- peso[ok]
    if (length(valor) < 2) return(NA_real_)
    media_pond <- sum(valor * peso) / sum(peso)
    sum_cuad <- sum((valor - media_pond)^2 * peso)
    if (tipo_var == "muestral") {
      denom <- sum(peso)
    } else {
      denom <- sum(peso) - 1
    }
    if (denom <= 0) return(NA_real_)
    sum_cuad / denom
  }
  
  # --- Cálculo final respetando grupos ---
  if (is.null(peso_name)) {
    # Sin pesos
    if (legacy_var) {
      result <- data %>%
        dplyr::select(dplyr::all_of(varnames)) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(),
                                       ~ var_no_pond(.x, tipo_var = tipo)))
    } else {
      result <- data %>%
        dplyr::summarise(dplyr::across({{ vars_expr }},
                                       ~ var_no_pond(.x, tipo_var = tipo)))
    }
  } else {
    # Con pesos: solo una variable
    if (legacy_var) {
      if (length(varnames) != 1) {
        stop("Solo puede calcularse la varianza ponderada para una variable a la vez")
      }
      var_name <- varnames[1]
    } else {
      sel_vars <- tryCatch(
        tidyselect::eval_select(vars_expr, data = data),
        error = function(e) stop("Selección inválida para ponderación")
      )
      if (length(sel_vars) != 1) {
        stop("Solo puede calcularse la varianza ponderada para una variable a la vez")
      }
      var_name <- names(sel_vars)[1]
    }
    
    if (var_name == peso_name) {
      stop("No puedes usar la misma variable como dato y como peso.")
    }
    
    result <- data %>%
      dplyr::summarise(
        !!var_name := var_pond(.data[[var_name]], .data[[peso_name]], tipo_var = tipo)
      )
  }
  
  class(result) <- c("resumen", class(result))
  return(result)
}
