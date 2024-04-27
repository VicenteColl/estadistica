library(dplyr)

media.nueva <- function(data, variable = NULL, pesos = NULL) {
  if (is.numeric(data) && is.numeric(variable) && is.null(pesos)) {
    # Trata los dos vectores como 'variable' y 'pesos'
    data <- data.frame(variable = data, pesos = variable)
    variable <- "variable"
    pesos <- "pesos"

    # Calcula directamente la media ponderada sin usar summarise()
    if (!is.null(pesos)) {
      datos_completos <- na.omit(data)
      media_ponderada <- sum(datos_completos$variable * datos_completos$pesos) / sum(datos_completos$pesos)
      return(data.frame(media_ponderada = media_ponderada))
    }
  } else if (is.numeric(data) && is.null(variable) && is.null(pesos)) {
    # Solo un vector, se trata como 'variable' sin pesos
    data <- data.frame(variable = data)
    variable <- "variable"
    # return(data.frame(media = mean(data$variable, na.rm = TRUE)))
  } else if (!is.data.frame(data)) {
    stop("El argumento data debe ser un dataframe o un vector numérico.")
  }

  # Ajusta la variable y pesos si se especifica índice numérico o no se especifica nada
  if (is.null(variable)) {
    variable <- names(data)[sapply(data, is.numeric)]
    if (is.grouped_df(data)) {
      group_cols <- group_vars(data)
      variable <- setdiff(variable, group_cols)
    }
  } else if (is.numeric(variable)) {
    variable <- names(data)[variable]
  } else if (!is.character(variable) || !all(variable %in% names(data))) {
    stop("El argumento 'variable' debe ser un nombre de columna, un índice numérico o un vector numérico.")
  }

  if (!is.null(pesos)) {
    if (is.numeric(pesos)) {
      pesos <- names(data)[pesos]
    } else if (!is.character(pesos) || !pesos %in% names(data)) {
      stop("El argumento 'pesos' debe ser un nombre de columna o un índice numérico.")
    }
    variable <- setdiff(variable, pesos)
  }

  # Realiza el cálculo usando summarise si no se entraron solo dos vectores
  results_df <- data %>%
    summarise(across(all_of(variable),
                     ~ if (is.null(pesos)) {
                       mean(.x, na.rm = TRUE)
                     } else {
                       datos_completos <- na.omit(data.frame(valores = .x, pesos = .data[[pesos]]))
                       sum(datos_completos$valores * datos_completos$pesos) / sum(datos_completos$pesos)
                     },
                     .names = if (is.null(pesos)) "media_{.col}" else "media_ponderada_{.col}"),
              .groups = "drop")

  return(results_df)
}

