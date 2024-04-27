library(dplyr)

desviacion.nueva <- function(data, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {
  # Validar y establecer el tipo de desviación
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

  # Ajustar variable y pesos si son introducidos como índices numéricos
  if (!is.null(variable) && is.numeric(variable)) {
    variable <- names(data)[variable]
  }

  if (!is.null(pesos) && is.numeric(pesos)) {
    pesos <- names(data)[pesos]
  }

  # Si variable no está definido, usar todas las variables numéricas o enteras
  if (is.null(variable)) {
    variable <- names(data)[sapply(data, function(x) is.numeric(x) || is.integer(x))]
    # Excluir las columnas de grupo si data es un dataframe agrupado
    if (is.grouped_df(data)) {
      group_cols <- group_vars(data)
      variable <- setdiff(variable, group_cols)
    }
  }

  # Excluir la columna de pesos de las variables si es necesario
  if (!is.null(pesos)) {
    variable <- setdiff(variable, pesos)
  }

  # Calcular la desviación estándar
  results_df <- data %>%
    summarise(across(all_of(variable),
                     ~ {
                       if (is.null(pesos)) {
                         # Desviación estándar muestral o cuasi
                         sd_x <- sd(.x, na.rm = TRUE)
                         n <- sum(!is.na(.x))
                         factor <- if (tipo == "muestral") sqrt((n - 1) / n) else 1
                         sd_x * factor
                       } else {
                         # Asegurar casos completos
                         datos_completos <- na.omit(data.frame(valores = .x, pesos = .data[[pesos]]))
                         # Desviación estándar ponderada
                         media_ponderada <- weighted.mean(datos_completos$valores, datos_completos$pesos, na.rm = TRUE)
                         sumatorio <- sum((datos_completos$valores - media_ponderada)^2 * datos_completos$pesos, na.rm = TRUE)
                         n <- sum(datos_completos$pesos, na.rm = TRUE)
                         factor <- if (tipo == "cuasi") 1 / (n - 1) else 1 / n
                         sqrt(sumatorio * factor)
                       }
                     },
                     .names = "desviacion_{.col}"),
              .groups = "drop")

  return(results_df)
}
