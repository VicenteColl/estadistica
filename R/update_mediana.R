library(dplyr)

# Función auxiliar para calcular la mediana
calculate_median <- function(x, pesos = NULL) {
  if (!is.null(pesos)) {
    # Asegurarse de que tanto x como pesos estén sincronizados y no contengan NA
    valid_indices <- !is.na(x) & !is.na(pesos)
    x <- x[valid_indices]
    pesos <- pesos[valid_indices]

    # Ordenar x y sus correspondientes pesos
    order_indices <- order(x)
    x <- x[order_indices]
    pesos <- pesos[order_indices]

    # Calcular la suma acumulada de pesos
    cumsum_pesos <- cumsum(pesos)
    total_weight <- sum(pesos)

    # Encontrar el índice donde la suma acumulada de pesos supera o iguala la mitad del peso total
    median_idx <- which(cumsum_pesos >= total_weight / 2)[1]
    return(x[median_idx])
  } else {
    # Mediana sin pesos
    return(median(x, na.rm = TRUE))
  }
}


# Función principal para calcular la mediana, usando calculate_median
mediana.nueva <- function(.data, variable = NULL, pesos = NULL) {

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


  # Determinar las variables sobre las que calcular la mediana
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
    variable <- setdiff(variable, pesos)  # Excluir la columna de pesos de las variables
  }

  # Usamos calculate_median para calcular la mediana
  results_df <- .data %>%
    summarise(across(all_of(variable),
                     ~ {
                       # Asegurarse de que los pesos son tratados como un vector que corresponde a x
                       weight_vec <- if (!is.null(pesos)) .data[[pesos]][!is.na(.x)] else NULL
                       calculate_median(.x[!is.na(.x)], weight_vec)
                     },
                     .names = "{.col}_quantile"),
              .groups = "drop")


  return(results_df)
}

