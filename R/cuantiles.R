#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcuantiles.png}{width = 200px}}
#' \if{latex}{\figure{qrcuantiles.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param cortes Vector con los puntos de corte a calcular. Por defecto se calcula el primer, segundo y tercer cuartil.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Si \code{pesos = NULL}, la función devuelve los cuantiles de todas las variables seleccionadas en un objeto de tipo \code{data.frame}. En caso contrario, devuelve los cuantiles de la variable para la que se ha facilitado la distribución de frecuencias.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' @details
#'
#' Los cuantiles se obtienen a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{cuantiles.png}{width = 680px}}
#' \if{latex}{\figure{cuantiles.png}{options: scale=.85}}
#'
#' Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' cuartiles: s=1,2,3 y k=4
#'
#' deciles: s= 1,2,...,9 y k=10
#'
#' percentiles: s=1,2,...,99 y k=100
#'
#' @seealso \code{\link{media}}, \code{\link{mediana}}
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
#' cuantiles1 <- cuantiles(startup[1])
#' cuantiles2 <- cuantiles(startup,variable=1,cortes=seq(0.1,0.9,0.1))
#' cuantiles3 <- cuantiles(salarios2018,variable=6,pesos=7 )
#'
#' @importFrom stats quantile
#'
#' @export
cuantiles <- function(data, variable = NULL, pesos = NULL,
                             cortes = c(0.25, 0.5, 0.75),
                             exportar = FALSE) {
  
  # Si no es data.frame, convertir (vector simple)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }
  
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
  
  # --- Verificar reglas de ponderación ---
  if (!is.null(peso_name)) {
    # Con pesos: solo se permite una variable
    if (legacy_var) {
      if (length(varnames) != 1) stop("Para cuantiles ponderados solo puedes seleccionar una variable")
    } else {
      # Evaluar la selección tidy para saber cuántas variables
      sel_vars <- tryCatch(
        tidyselect::eval_select(vars_expr, data = data),
        error = function(e) stop("Selección inválida")
      )
      if (length(sel_vars) != 1) stop("Para cuantiles ponderados solo puedes seleccionar una variable")
    }
    # En el flujo con pesos, se manejará más adelante
  }
  
  # --------------------------------------------------------------------
  # Cálculo de cuantiles respetando grupos
  # --------------------------------------------------------------------
  
  # Función que aplica .cuantiles.int a una(s) variable(s) (sin pesos o con pesos)
  compute_quantiles <- function(df, vars, pesos_name = NULL, cortes) {
    # df: data frame (puede ser grouped o no)
    # vars: vector de nombres de columnas a evaluar
    # pesos_name: nombre de columna de pesos (o NULL)
    # Retorna un data frame con columnas: variable, prob, value
    if (is.null(pesos_name)) {
      # Sin pesos: calcular cuantiles para cada variable
      res_list <- list()
      for (v in vars) {
        q_df <- .cuantiles.int(df[[v]], pesos = NULL, cortes = cortes)
        q_df$variable <- v
        res_list[[v]] <- q_df
      }
      result <- bind_rows(res_list)
    } else {
      # Con pesos: solo una variable
      v <- vars[1]
      q_df <- .cuantiles.int(df[[v]], pesos = df[[pesos_name]], cortes = cortes)
      q_df$variable <- v
      result <- q_df
    }
    return(result)
  }
  
  # Determinar las columnas de variable a usar (según legacy/tidy)
  if (legacy_var) {
    selected_vars <- varnames
  } else {
    # Evaluar la expresión tidy para obtener nombres de columnas
    selected_vars <- names(tidyselect::eval_select(vars_expr, data = data))
  }
  
  # Verificar que sean numéricas (ya lo hace .cuantiles.int internamente, pero podemos advertir)
  # No es necesario porque la función interna lanzará error.
  
  # Si hay grupos (grouped data frame)
  if (inherits(data, "grouped_df")) {
    # Trabajar con grupos
    group_vars <- dplyr::group_vars(data)
    # Aplicar compute_quantiles a cada grupo
    result <- data %>%
      dplyr::group_modify(~ {
        .x %>%
          compute_quantiles(vars = selected_vars,
                            pesos_name = peso_name,
                            cortes = cortes)
      }) %>%
      dplyr::ungroup()
    
    # Reorganizar: queremos una columna por cada variable? o mantener formato largo?
    # Dado que la salida original es ancha (variables como columnas, filas = cuantiles),
    # para grupos lo más útil es mantener el formato largo con columnas de grupo, prob, variable, value.
    # Dejamos así.
    
  } else {
    # Sin grupos: comportamiento original (ancho: columnas = variables, filas = cuantiles)
    if (is.null(peso_name)) {
      # Múltiples variables sin pesos
      # Aplicar .cuantiles.int a cada columna y luego combinar en matriz
      quantiles_list <- list()
      for (v in selected_vars) {
        q_df <- .cuantiles.int(data[[v]], pesos = NULL, cortes = cortes)
        quantiles_list[[v]] <- q_df$value
      }
      quantiles_mat <- do.call(cbind, quantiles_list)
      colnames(quantiles_mat) <- paste0("cuantiles_", selected_vars)
      rownames(quantiles_mat) <- paste0(cortes * 100, "%")
      result <- as.data.frame(quantiles_mat)
    } else {
      # Con pesos: una sola variable
      v <- selected_vars[1]
      q_df <- .cuantiles.int(data[[v]], pesos = data[[peso_name]], cortes = cortes)
      result <- as.data.frame(q_df$value)
      rownames(result) <- paste0(cortes * 100, "%")
      colnames(result) <- paste0("cuantiles_", v)
    }
  }
  
  # --- Exportar a Excel si se solicita ---
  if (exportar) {
    filename <- paste0("Cuantiles_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Cuantiles")
    
    if (inherits(data, "grouped_df")) {
      # Para datos agrupados, el resultado ya es un data.frame largo con grupos.
      # Lo escribimos directamente, añadiendo formato numérico a las columnas de valores.
      res_export <- result
      # Asegurar que la columna 'value' tenga formato numérico con 4 decimales
      openxlsx::writeData(wb, "Cuantiles", res_export)
      # Aplicar formato a las columnas numéricas (value)
      value_col <- which(names(res_export) == "value")
      if (length(value_col) > 0) {
        openxlsx::addStyle(wb, "Cuantiles",
                           style = openxlsx::createStyle(numFmt = "0.0000"),
                           rows = 2:(nrow(res_export) + 1),
                           cols = value_col,
                           gridExpand = TRUE)
      }
    } else {
      # Formato ancho original
      res_export <- cbind(Cuantil = rownames(result), result)
      rownames(res_export) <- NULL
      openxlsx::writeData(wb, "Cuantiles", res_export)
      # Formato numérico a todas las columnas excepto la primera
      openxlsx::addStyle(wb, "Cuantiles",
                         style = openxlsx::createStyle(numFmt = "0.0000"),
                         rows = 2:(nrow(res_export) + 1),
                         cols = 2:(ncol(res_export)),
                         gridExpand = TRUE)
    }
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }
  
  class(result) <- c("resumen", class(result))
  return(result)
}
