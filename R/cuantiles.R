#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcuantiles.png}{options: style="width: 25\%;"}}
#' \if{latex}{\figure{qrcuantiles.png}{options: width=3cm}}
#'
#' @param data Conjunto de datos. Puede ser un vector o un dataframe.
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
#' Si:
#'
#' \deqn{
#' \left\{
#' \begin{array}{lll}
#' N_{i-1}<\displaystyle\frac{s \cdot n}{k}<N_i
#' & \Rightarrow &
#' Q_{\frac{s}{k}}=x_i
#' \\
#' \\
#' N_i=\displaystyle\frac{s \cdot n}{k}
#' & \Rightarrow &
#' Q_{\frac{s}{k}}=\displaystyle\frac{x_i+x_{i+1}}{2}
#' \end{array}
#' \right.
#' }
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

  # Convertir dataframae (vector simple)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }

  # Capturar argumentos
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)

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
        error = function(e) stop("El argumento 'pesos' debe ser el nombre o \u00edndice de una variable")
      )
      if (!peso_name %in% names(data)) stop("La columna de pesos no existe en los datos")
    }
  }


  if (!is.null(peso_name)) {
    # Con pesos: solo se permite una variable
    if (legacy_var) {
      if (length(varnames) != 1) stop("Para cuantiles ponderados solo puedes seleccionar una variable")
    } else {
      sel_vars <- tryCatch(
        tidyselect::eval_select(vars_expr, data = data),
        error = function(e) stop("Selecci\u00f3n no v\u00e1lida")
      )
      if (length(sel_vars) != 1) stop("Para cuantiles ponderados solo puedes seleccionar una variable")
    }
  }

  # calcular cuantiles
  compute_quantiles <- function(df, vars, pesos_name = NULL, cortes) {
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

  # Columnas a usar
  if (legacy_var) {
    selected_vars <- varnames
  } else {
    # Evaluar la expresion tidy para obtener nombres de columnas
    selected_vars <- names(tidyselect::eval_select(vars_expr, data = data))
  }

  # Si hay grupos
  if (inherits(data, "grouped_df")) {
    group_vars <- dplyr::group_vars(data)
    result <- data %>%
      dplyr::group_modify(~ {
        .x %>%
          compute_quantiles(vars = selected_vars,
                            pesos_name = peso_name,
                            cortes = cortes)
      }) %>%
      dplyr::ungroup()

  } else {
    # Sin grupos
    if (is.null(peso_name)) {
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

  # Exportar excel
  if (exportar) {
    filename <- paste0("Cuantiles_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Cuantiles")

    if (inherits(data, "grouped_df")) {
      res_export <- result
      # Asegurar que la columna 'value' tenga formato numerico con 4 decimales
      openxlsx::writeData(wb, "Cuantiles", res_export)
      # Aplicar formato a las columnas numericas
      value_col <- which(names(res_export) == "value")
      if (length(value_col) > 0) {
        openxlsx::addStyle(wb, "Cuantiles",
                           style = openxlsx::createStyle(numFmt = "0.0000"),
                           rows = 2:(nrow(res_export) + 1),
                           cols = value_col,
                           gridExpand = TRUE)
      }
    } else {
      res_export <- cbind(Cuantil = rownames(result), result)
      rownames(res_export) <- NULL
      openxlsx::writeData(wb, "Cuantiles", res_export)
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
