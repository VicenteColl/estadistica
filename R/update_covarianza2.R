library(dplyr)

covarianza.nueva <- function(data, variable, pesos = NULL, tipo = "muestral") {
  tipo <- match.arg(tolower(tipo), choices = c("muestral", "cuasi"))

  # Validar que se especificaron exactamente dos variables
  if (length(variable) != 2) {
    stop("Debes especificar exactamente dos variables para calcular la covarianza.")
  }

  # Verificar que las variables y pesos existan en el dataframe
  if (!all(variable %in% names(data))) {
    stop("Una o más variables especificadas no existen en el dataframe.")
  }
  if (!is.null(pesos) && !pesos %in% names(data)) {
    stop("La variable de pesos especificada no existe en el dataframe.")
  }

  # Extraer solo las columnas necesarias
  cols <- c(variable, if (!is.null(pesos)) pesos else NULL)
  data <- select(data, all_of(cols))

  # Verificar que todas las columnas sean numéricas
  if (!all(sapply(data, is.numeric))) {
    stop("Todas las variables y los pesos deben ser numéricos.")
  }

  # Calcular la covarianza
  covarianza <- summarise(data, {
    if (is.null(pesos)) {
      # Covarianza sin ponderar
      n <- nrow(data)
      factor <- if (tipo == "muestral") (n - 1) / n else 1
      cov_val <- cov(data[[variable[1]]], data[[variable[2]]], use = "everything") * factor
    } else {
      # Covarianza ponderada
      weights <- data[[pesos]]
      mean1 <- weighted.mean(data[[variable[1]]], weights)
      mean2 <- weighted.mean(data[[variable[2]]], weights)
      sum_weights <- sum(weights)
      factor <- if (tipo == "muestral") sum_weights else (sum_weights - 1)
      cov_val <- sum((data[[variable[1]]] - mean1) * (data[[variable[2]]] - mean2) * weights) / factor
    }
    round(cov_val, 4)
  }, .groups = "drop")

  # Asignar nombre al resultado
  names(covarianza) <- paste("covarianza", variable[1], variable[2], sep = "_")
  return(covarianza)
}
