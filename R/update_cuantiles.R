library(dplyr)
library(tidyr)

cuantiles_int <- function(x, pesos = NULL, cortes = c(0.25, 0.5, 0.75)) {
  if (!is.null(pesos)) {
    valid_indices <- !is.na(x) & !is.na(pesos)
    x <- x[valid_indices]
    pesos <- pesos[valid_indices]
  } else {
    x <- na.omit(x)
    pesos <- rep(1, length(x))
  }

  if (length(x) == 0) {
    return(setNames(rep(NA, length(cortes)), paste0(cortes * 100, "%")))
  }

  df <- data.frame(x, pesos)
  df <- df[order(df$x),]
  df$cumsum_pesos <- cumsum(df$pesos)
  total_weight <- sum(df$pesos)

  cuantiles <- sapply(cortes, function(corte) {
    threshold = total_weight * corte
    idx = which(df$cumsum_pesos >= threshold)[1]
    if (length(idx) == 0) return(NA)  # Manejo de índice vacío
    return(df$x[idx])
  })

  names(cuantiles) <- paste0(cortes * 100, "%")
  return(cuantiles)
}


cuantiles <- function(.data, variable = NULL, pesos = NULL, cortes = c(0.25, 0.5, 0.75)) {

  # Si .data es un vector numérico, transformarlo en un dataframe
  if (is.numeric(.data) && is.null(variable) && is.null(pesos)) {
    # Un vector sin pesos
    .data <- data.frame(variable = .data)
    variable= "variable"
  } else if (is.numeric(.data) && is.numeric(variable) && is.null(pesos) ) {
    .data <- data.frame(variable = .data, pesos= variable)
    variable <- "variable"
    pesos <- "pesos"

  } else if (is.numeric(.data) && is.numeric(variable) && is.numeric(pesos)) {
    stop("Ha introducido más de dos vectores")

  } else if (!is.data.frame(.data)) {
    stop("El argumento .data debe ser un dataframe o un vector numérico.")
  }


  # Determinar las variables sobre las que calcular cuantiles
  if (is.null(variable)) {
    variable <- names(.data)[sapply(.data, is.numeric)]  # selecciona solo las columnas numéricas
  } else if (is.numeric(variable)) {
    if (any(variable > length(.data) || variable < 1)) {
      stop("Índice de 'variable' fuera de rango.")
    }
    variable <- names(.data)[variable]  # Convierte índices en nombres de columnas
  } else if (!all(variable %in% names(.data))) {
    stop("Una o más variables especificadas no existen en '.data'.")
  }

  # Ajustar el argumento pesos para que sea el nombre de la columna
  if (!is.null(pesos)) {
    if (is.numeric(pesos)) {
      if (pesos > length(.data) || pesos < 1) {
        stop("Índice de 'pesos' fuera de rango.")
      }
      pesos <- names(.data)[pesos]  # Convierte índices en nombres de columnas
    } else if (!pesos %in% names(.data)) {
      stop("El argumento 'pesos' debe ser un nombre de columna válido en '.data'.")
    }
  }

  results_df <- .data %>%
    summarise(across(all_of(variable),
                     ~ {
                       # Asegurarse de que los pesos son tratados como un vector que corresponde a x
                       weight_vec <- if (!is.null(pesos)) .data[[pesos]][!is.na(.x)] else NULL
                       cuantiles_int(.x[!is.na(.x)], weight_vec, cortes)
                     },
                     .names = "{.col}_quantile"),
              .groups = "drop")

  return(results_df)
}

