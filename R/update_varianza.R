library(dplyr)

varianza.nueva <- function(data, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {
  # Validar y establecer el tipo de varianza
  tipo <- match.arg(tolower(tipo), choices = c("muestral", "cuasi"))

  # Si data es un vector numérico, transformarlo en un dataframe
  if (is.numeric(data) && is.null(variable) && is.null(pesos)) {
    # Un vector sin pesos
    data <- data.frame(variable = data)
    variable= "variable"
  } else if (is.numeric(data) && is.numeric(variable) && is.null(pesos) ) {
    data <- data.frame(variable = data, pesos= variable)
    variable <- "variable"
    pesos <- "pesos"

  } else if (is.numeric(data) && is.numeric(variable) && is.numeric(pesos)) {
    stop("Ha introducido más de dos vectores")

  } else if (!is.data.frame(data)) {
    stop("El argumento data debe ser un dataframe o un vector numérico.")
  }

  # Ajustar variable si es introducida como índice numérico o seleccionar todas las numéricas si no se especifica
  if (is.null(variable)) {
    variable <- names(data)[sapply(data, is.numeric)]
    # Excluir las columnas de grupo si data es un dataframe agrupado
    if (is.grouped_df(data)) {
      group_cols <- group_vars(data)
      variable <- setdiff(variable, group_cols)
    }
  } else if (is.numeric(variable)) {
    variable <- names(data)[variable]
  } else if (!is.character(variable) || !all(variable %in% names(data))) {
    stop("El argumento 'variable' debe ser un nombre de columna, un índice numérico o un vector numérico.")
  }

  # Ajustar pesos si es introducido como índice numérico
  if (!is.null(pesos)) {
    if (is.numeric(pesos)) {
      pesos <- names(data)[pesos]
    } else if (!is.character(pesos) || !pesos %in% names(data)) {
      stop("El argumento 'pesos' debe ser un nombre de columna o un índice numérico.")
    }
    variable <- setdiff(variable, pesos)
  }

  # Calcular la varianza
  results_df <- data %>%
    summarise(across(all_of(variable),
                     ~ {
                       if (is.null(pesos)) {
                         # Varianza muestral o cuasi-varianza
                         var_factor <- if (tipo == "muestral") (length(.x) - 1) / length(.x) else 1
                         var(.x, na.rm = TRUE) * var_factor
                       } else {
                         # Asegurar casos completos
                         datos_completos <- na.omit(data.frame(valores = .x, pesos = .data[[pesos]]))
                         # Varianza ponderada
                         media_ponderada <- weighted.mean(datos_completos$valores, datos_completos$pesos, na.rm = TRUE)
                         sumatorio <- sum((datos_completos$valores - media_ponderada)^2 * datos_completos$pesos, na.rm = TRUE)
                         n <- sum(datos_completos$pesos, na.rm = TRUE)
                         var_factor <- if (tipo == "cuasi") 1 / (n - 1) else 1 / n
                         sumatorio * var_factor
                       }
                     },
                     .names = "varianza_{.col}"),
              .groups = "drop")

  return(results_df)
}

