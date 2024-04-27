library(dplyr)

covarianza.nueva <- function(data, variable = NULL, pesos = NULL, tipo = "muestral") {
  tipo <- match.arg(tolower(tipo), choices = c("muestral", "cuasi"))

  # Determinar si los datos están agrupados
  grouped <- length(group_vars(data)) > 0

  # Validar la entrada de las variables y pesos
  if (is.null(variable)) {
    if (!is.null(pesos) && ncol(data) != 3) {
      stop("Debes proporcionar exactamente tres variables en el dataframe cuando se especifican los pesos.")
    } else if (is.null(pesos) && ncol(data) != 2) {
      stop("Debes proporcionar exactamente dos variables en el dataframe cuando no se especifican los pesos.")
    }
    variable <- names(data)[1:2]  # Seleccionar automáticamente las primeras dos columnas
  } else {
    if (is.numeric(variable)) {
      if (any(variable > ncol(data))) {
        stop("Índices de variables fuera de rango.")
      }
      variable <- names(data)[variable]
    } else if (!all(variable %in% names(data))) {
      stop("Una o más variables especificadas no existen en el dataframe.")
    }

    if (!is.null(pesos)) {
      if (is.numeric(pesos)) {
        if (pesos > ncol(data)) {
          stop("Índice de pesos fuera de rango.")
        }
        pesos <- names(data)[pesos]
      } else if (!pesos %in% names(data)) {
        stop("La variable de pesos especificada no existe en el dataframe.")
      }

      if (pesos %in% variable) {
        stop("La variable de pesos no puede ser una de las variables de covarianza.")
      }
    }
  }

  # Calcular la covarianza
  resultado <- summarise(data, {
    current_data <- cur_data()  # Referencia al dataframe en el contexto actual
    x <- current_data[[variable[1]]]
    y <- current_data[[variable[2]]]
    if (is.null(pesos)) {
      # Covarianza sin ponderar
      n <- length(x)
      factor = if (tipo == "cuasi" && n > 1) 1 else (n-1)/n
      cov_val = cov(x, y, use = "complete.obs") * factor
    } else {
      # Covarianza ponderada
      weights = current_data[[pesos]]
      mean_x = weighted.mean(x, weights, na.rm = TRUE)
      mean_y = weighted.mean(y, weights, na.rm = TRUE)
      sum_weights = sum(weights)
      factor = if (tipo == "cuasi" && sum_weights > 0) sum_weights / (sum_weights - 1) else 1
      sum_weighted_dev = sum((x - mean_x) * (y - mean_y) * weights, na.rm = TRUE)
      cov_val = sum_weighted_dev / (if (tipo == "cuasi") sum_weights else (sum_weights - 1)) * factor
    }
    round(cov_val, 4)
  }, .groups = if (grouped) "drop" else NULL)  # Mantener o no el agrupamiento

  # Asignar nombre al resultado
  names(resultado) <- paste("covarianza", variable[1], variable[2], sep = "_")
  return(resultado)
}
