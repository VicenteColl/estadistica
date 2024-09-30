library(dplyr)
library(purrr)  # Para usar map y otras funciones útiles

moda.nueva <- function(.data, variable = NULL, pesos = NULL) {
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
  # Ajuste para nombres de variables y pesos
  if (!is.null(pesos)) {
    if (is.numeric(pesos)) {
      if (any(pesos > length(names(.data)))) {
        stop("Índice de pesos fuera de los límites.")
      }
      pesos <- names(.data)[pesos]
    } else if (!is.numeric(.data[[pesos]])) {
      stop("La variable de pesos debe ser numérica o entera.")
    }
  }

  if (is.null(variable)) {
    variable <- names(.data)
    if (!is.null(pesos)) {
      variable <- variable[variable != pesos]
    }
  } else if (is.numeric(variable)) {
    if (any(variable > length(names(.data)))) {
      stop("Índice de variable fuera de los límites.")
    }
    variable <- names(.data)[variable]
  }

  calculate_mode <- function(data_col, weights=NULL) {
    if (!is.null(weights)) {
      weights <- weights[!is.na(data_col)]
      data_col <- data_col[!is.na(data_col)]
      weighted_table <- tapply(weights, data_col, FUN = sum)
      max_val <- max(weighted_table)
      modes <- names(weighted_table[weighted_table == max_val])
    } else {
      freq_table <- table(data_col)
      max_val <- max(freq_table)
      modes <- names(freq_table[freq_table == max_val])
    }
    if (length(modes) == length(unique(data_col))) {
      return(NA)
    } else {
      return(modes)
    }
  }

  # Caso agrupado
  if ("grouped_df" %in% class(.data)) {
    moda_results <- .data %>%
      summarise(across(all_of(variable), ~list(calculate_mode(.x, if (!is.null(pesos)) .data[[pesos]] else NULL)), .names = "moda_{.col}")) %>%
      unnest(everything())
    return(moda_results)
  }

  # Caso no agrupado
  max_length <- 0
  results <- list()
  for (var in variable) {
    data_col <- .data[[var]]
    weights_col <- if (!is.null(pesos)) .data[[pesos]] else NULL
    moda_result <- calculate_mode(data_col, weights_col)
    results[[var]] <- moda_result
    max_length <- max(max_length, length(moda_result))
  }

  # Estándar rellenando con NA para que todos los resultados tengan la misma longitud
  results <- lapply(results, function(x) {
    length(x) <- max_length
    x
  })

  moda_df <- as.data.frame(results)
  names(moda_df) <- paste("moda", variable, sep="_")

  return(moda_df)
}
