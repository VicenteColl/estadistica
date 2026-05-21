#' @title Medidas de forma
#'
#' @description Calcula el coeficiente de asimetría y de curtosis de Fisher.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrforma.png}{width = 200px}}
#' \if{latex}{\figure{qrforma.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos, que puede estar formado por una o más variables.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param alternativa Es un valor lógico. Si alternativa = TRUE el resultado de las medidas de forma muestra el coeficiente de asimetría y curtosis calculado según SPSS y EXCEL. Se facilita también los correspondientes errores típicos. Este argumento no funciona si pesos = NULL.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
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
#' El coeficiente de asimetría se obtiene a partir de la expresión:
#'
#' \if{html}{\figure{asimetriamuestra.png}{width = 160px}}
#' \if{latex}{\figure{asimetriamuestra.png}{options: width=3cm}}
#'
#' y el coeficiente de curtosis:
#'
#' \if{html}{\figure{curtosismuestra.png}{width = 280px}}
#' \if{latex}{\figure{curtosismuestra.png}{options: width=4cm}}
#'
#' @note
#' (1) El coeficiente de asimetría poblacional es:
#'
#' \if{html}{\figure{asimetriapob.png}{width = 160px}}
#' \if{latex}{\figure{asimetriapob.png}{options: width=3cm}}
#'
#' (2) El coeficiente de curtosis poblacional es:
#'
#' \if{html}{\figure{curtosispob.png}{width = 2800px}}
#' \if{latex}{\figure{curtosispob.png}{options: width=4cm}}
#'
#' (3) Si el argumento alternativa = TRUE, se obtienen los resultados de asimetría y curtosis que generalmente ofrecen softwares como: SPSS, Stata, SAS, Excel, etc.
#'
#'
#' \if{html}{\figure{asimetriasoft.png}{width = 4800px}}
#' \if{latex}{\figure{asimetriasoft.png}{options: width=8cm}}
#'
#'  \if{html}{\figure{curtosissoft.png}{width = 920px}}
#' \if{latex}{\figure{curtosissoft.png}{options: width=13cm}}
#'
#' @seealso \code{\link{varianza}},\code{\link{desviacion}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom stats complete.cases
#' @examples
#'
#' forma <- medidas.forma(startup)
#' forma2 <- medidas.forma(startup, alternativa= TRUE)
#'
#' @export
medidas.forma <- function(data, variable = NULL, pesos = NULL,
                          alternativa = FALSE, exportar = FALSE) {

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
    eval_res <- tryCatch(eval_tidy(var_quo, env = caller_env()),
                         error = function(e) NULL)
    if (is.numeric(eval_res) || is.character(eval_res)) {
      legacy_var <- TRUE
      if (is.numeric(eval_res)) {
        if (any(eval_res > ncol(data))) stop("Selección errónea de variables")
        varnames <- names(data)[eval_res]
      } else {
        if (!all(eval_res %in% names(data))) stop("Nombre de variable no válido")
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
    pesos_eval <- tryCatch(eval_tidy(pesos_quo, env = caller_env()),
                           error = function(e) NULL)
    if (is.numeric(pesos_eval) || is.character(pesos_eval)) {
      if (is.numeric(pesos_eval)) {
        if (pesos_eval > ncol(data)) stop("Selección errónea de pesos")
        peso_name <- names(data)[pesos_eval[1]]
      } else {
        if (!(pesos_eval[1] %in% names(data))) stop("Nombre de pesos no válido")
        peso_name <- pesos_eval[1]
      }
    } else {
      peso_name <- tryCatch(as_name(pesos_quo),
                            error = function(e) stop("El argumento 'pesos' debe ser una columna única o su nombre/índice"))
      if (!peso_name %in% names(data)) stop("La columna de pesos no existe en los datos")
    }
  }

  # --- Validar reglas de ponderación ---
  if (!is.null(peso_name)) {
    if (legacy_var) {
      if (length(varnames) != 1) stop("Para medidas de forma ponderadas solo puedes seleccionar una variable")
    } else {
      sel_vars <- tryCatch(tidyselect::eval_select(vars_expr, data = data),
                           error = function(e) stop("Selección inválida para ponderación"))
      if (length(sel_vars) != 1) stop("Para medidas de forma ponderadas solo puedes seleccionar una variable")
    }
  }

  # --- Obtener nombres de las columnas seleccionadas ---
  if (legacy_var) {
    selected_vars <- varnames
  } else {
    selected_vars <- names(tidyselect::eval_select(vars_expr, data = data))
  }

  # --- Advertencia sobre alternativa con grupos ---
  has_groups <- inherits(data, "grouped_df")
  if (alternativa && has_groups) {
    message("La opción 'alternativa = TRUE' solo está implementada para datos no agrupados. Se ignorará.")
    alternativa <- FALSE
  }

  # ------------------------------------------------------------------
  # Funciones auxiliares para el cálculo (sin usar group_modify internamente)
  # Estas funciones devuelven un data.frame de una fila con las estadísticas
  # ------------------------------------------------------------------
  compute_shape_no_weights <- function(df, vars) {
    # df: data.frame (puede ser un grupo, pero se usa como tal)
    # vars: vector de nombres de columnas numéricas
    # Retorna data.frame de 1 fila con columnas: asimetria_var1, curtosis_var1, ...
    res <- list()
    for (v in vars) {
      x <- df[[v]]
      x <- x[!is.na(x)]
      if (length(x) < 3) {
        res[[paste0("asimetria_", v)]] <- NA_real_
        res[[paste0("curtosis_", v)]] <- NA_real_
        next
      }
      momento3 <- .momento.central(data.frame(x), orden = 3)[1]
      momento4 <- .momento.central(data.frame(x), orden = 4)[1]
      desv <- sd(x)
      asimetria <- momento3 / desv^3
      curtosis <- momento4 / desv^4 - 3
      res[[paste0("asimetria_", v)]] <- asimetria
      res[[paste0("curtosis_", v)]] <- curtosis
    }
    as.data.frame(res)
  }

  compute_shape_with_weights <- function(df, var, peso) {
    x <- df[[var]]
    w <- df[[peso]]
    ok <- !is.na(x) & !is.na(w)
    x <- x[ok]
    w <- w[ok]
    if (length(x) < 3) {
      return(data.frame(asimetria = NA_real_, curtosis = NA_real_))
    }
    media <- sum(x * w) / sum(w)
    m3 <- sum((x - media)^3 * w) / sum(w)
    m4 <- sum((x - media)^4 * w) / sum(w)
    desv <- desviacion(df, variable = !!rlang::sym(var), pesos = !!rlang::sym(peso), tipo = "cuasi")
    desv <- desv[[1]]
    asimetria <- m3 / desv^3
    curtosis <- m4 / desv^4 - 3
    data.frame(asimetria = asimetria, curtosis = curtosis)
  }

  # ------------------------------------------------------------------
  # Cálculo principal
  # ------------------------------------------------------------------
  if (is.null(peso_name)) {
    # Sin pesos
    if (has_groups) {
      result <- data %>%
        dplyr::group_modify(~ compute_shape_no_weights(.x, selected_vars)) %>%
        dplyr::ungroup()
    } else {
      result <- compute_shape_no_weights(data, selected_vars)
    }
  } else {
    # Con pesos (una variable)
    var_name <- selected_vars[1]
    if (has_groups) {
      result <- data %>%
        dplyr::group_modify(~ compute_shape_with_weights(.x, var_name, peso_name)) %>%
        dplyr::ungroup()
      # Renombrar columnas asimetria -> asimetria_var, curtosis -> curtosis_var
      names(result)[names(result) == "asimetria"] <- paste0("asimetria_", var_name)
      names(result)[names(result) == "curtosis"] <- paste0("curtosis_", var_name)
    } else {
      result <- compute_shape_with_weights(data, var_name, peso_name)
      names(result) <- c(paste0("asimetria_", var_name), paste0("curtosis_", var_name))
    }
  }

  # ------------------------------------------------------------------
  # Transformación final según haya grupos o no
  # ------------------------------------------------------------------
  if (!has_groups) {
    # Caso sin grupos: convertir a formato filas (asimetría, curtosis) o alternativa
    if (!alternativa) {
      # Formato clásico: dos filas, columnas = variables
      asim_row <- sapply(selected_vars, function(v) result[[paste0("asimetria_", v)]])
      curt_row <- sapply(selected_vars, function(v) result[[paste0("curtosis_", v)]])
      result <- rbind(asim_row, curt_row)
      rownames(result) <- c("asimetria", "curtosis")
      colnames(result) <- selected_vars
      result <- as.data.frame(result)
    } else {
      # Modo alternativa: generar tabla con N, asimetría alternativa, errores, etc.
      alt_list <- list()
      for (v in selected_vars) {
        x <- data[[v]]
        x <- x[!is.na(x)]
        n <- length(x)
        if (n < 4) {
          alt_df <- data.frame(
            N = n,
            asimetria_muestral = result[[paste0("asimetria_", v)]],
            asimetria_alt = NA_real_,
            error_asimetria = NA_real_,
            curtosis_muestral = result[[paste0("curtosis_", v)]],
            curtosis_alt = NA_real_,
            error_curtosis = NA_real_
          )
        } else {
          c1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
          c3 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
          error_asim <- sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))
          error_curt <- 2 * sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3))) *
            sqrt((n^2 - 1) / ((n - 3) * (n + 5)))
          m3 <- .momento.central(data.frame(x), orden = 3)[1]
          m4 <- .momento.central(data.frame(x), orden = 4)[1]
          desv <- sd(x)
          c2 <- (n * m4) / desv^4
          asim_alt <- (n / ((n - 1) * (n - 2))) * (n * m3) / desv^3
          curt_alt <- c1 * c2 - c3
          alt_df <- data.frame(
            N = n,
            asimetria_muestral = result[[paste0("asimetria_", v)]],
            asimetria_alt = asim_alt,
            error_asimetria = error_asim,
            curtosis_muestral = result[[paste0("curtosis_", v)]],
            curtosis_alt = curt_alt,
            error_curtosis = error_curt
          )
        }
        alt_list[[v]] <- alt_df
      }
      alt_result <- dplyr::bind_rows(alt_list, .id = "variable")
      # Reorganizar: filas = estadísticas, columnas = variables
      stats_order <- c("N", "asimetria_muestral", "asimetria_alt", "error_asimetria",
                       "curtosis_muestral", "curtosis_alt", "error_curtosis")
      final_rows <- list()
      for (stat in stats_order) {
        row_vals <- sapply(selected_vars, function(v) alt_result[alt_result$variable == v, stat])
        final_rows[[stat]] <- row_vals
      }
      result <- as.data.frame(do.call(rbind, final_rows))
      rownames(result) <- c("N", "Asimetría (muestral)", "Asimetría (alternativa)", "Error asimetría (alt)",
                            "Curtosis (muestral)", "Curtosis (alternativa)", "Error curtosis (alt)")
      colnames(result) <- selected_vars
    }
  } else {
    # Caso con grupos: ya está en formato ancho con grupos. No se transforma.
    # Solo redondeamos y ya.
    result <- result
  }

  # Redondear valores numéricos a 4 decimales
  result <- result %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # Exportar a Excel si se solicita
  if (exportar) {
    filename <- paste0("Medidas_de_forma_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Medidas_de_forma")
    if (has_groups || (!has_groups && !alternativa)) {
      resumen_export <- cbind(Estadística = rownames(result), result)
      rownames(resumen_export) <- NULL
    } else {
      resumen_export <- result
      rownames(resumen_export) <- NULL
    }
    openxlsx::writeData(wb, "Medidas_de_forma", resumen_export)
    # Aplicar formato numérico a todas las columnas excepto la primera (nombres de fila)
    if (ncol(resumen_export) > 1) {
      openxlsx::addStyle(wb, "Medidas_de_forma",
                         style = openxlsx::createStyle(numFmt = "0.0000"),
                         rows = 2:(nrow(resumen_export) + 1),
                         cols = 2:(ncol(resumen_export) + 1),
                         gridExpand = TRUE)
    }
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  class(result) <- c("resumen", class(result))
  return(result)
}
