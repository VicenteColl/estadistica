library(dplyr)
library(stats)  # Para usar cov.wt

# Función matriz.covar para calcular covarianzas con y sin datos agrupados
matriz.covar2 <- function(data, variable = NULL, tipo = c("muestral", "cuasi"), pesos = NULL) {
  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  # Ajuste de variables por índices o nombres
  selected_vars <- TRUE
  if (!is.null(variable)) {
    if (is.numeric(variable)) {
      if (all(variable <= ncol(data))) {
        selected_vars <- variable
      } else {
        stop("Indices de variables fuera de rango.")
      }
    } else if (is.character(variable)) {
      if (all(variable %in% names(data))) {
        selected_vars <- variable
      } else {
        stop("Nombres de variables no encontrados en los datos.")
      }
    }
  } else {
    selected_vars <- names(data)
  }

  # Verificar que todas las variables seleccionadas son cuantitativas
  if (!all(sapply(data[, selected_vars, drop = FALSE], is.numeric))) {
    stop("No puede calcularse la matriz de varianzas-covarianzas; alguna variable seleccionada no es cuantitativa.")
  }

  # Preparar los pesos
  if (!is.null(pesos)) {
    if (is.numeric(pesos) && length(pesos) == 1) {
      weights <- rep(pesos, nrow(data))
    } else if (is.numeric(pesos) && length(pesos) == nrow(data)) {
      weights <- pesos
    } else if (is.character(pesos) && pesos %in% names(data)) {
      weights <- data[[pesos]]
    } else {
      stop("Los pesos deben ser un número, un vector numérico de la longitud de las filas, o un nombre/índice de columna válido.")
    }
  } else {
    weights <- rep(1, nrow(data))  # Pesos iguales si no se especifican
  }

  # Cálculo de covarianza para datos agrupados o no agrupados
  if ("grouped_df" %in% class(data)) {
    # Calcular la matriz de covarianza para cada grupo
    result <- data %>%
      summarise(
        cov_mat = list({
          cur_data <- cur_data()[, selected_vars, drop = FALSE]
          cur_weights <- if (!is.null(pesos)) {
            if (is.character(pesos) && pesos %in% names(cur_data)) {
              cur_data[[pesos]]
            } else {
              weights[cur_group_rows()]
            }
          } else {
            rep(1, n())
          }
          cov_mat <- cov.wt(cur_data, wt = cur_weights, method = if (tipo == "cuasi") "ML" else "unbiased")
          matrix(cov_mat$cov, nrow = length(selected_vars), ncol = length(selected_vars))  # Asegurar que la salida es una matriz
        }),
        .groups = 'drop'  # Evitar regrouping
      ) %>%
      pull(cov_mat)  # Extraer solo la lista de matrices de covarianza
  } else {
    # Cálculo de la matriz de covarianza para datos no agrupados
    data_selected <- data[, selected_vars, drop = FALSE]
    cov_mat <- cov.wt(data_selected, wt = weights, method = if (tipo == "cuasi") "ML" else "unbiased")
    result <- matrix(cov_mat$cov, nrow = length(selected_vars), ncol = length(selected_vars))
  }

  return(result)
}


# Sin agrupación, especificando variables y pesos
result_no_group <- matriz.covar2(mtcars, variable = c("mpg", "disp", "hp"), tipo = "cuasi", pesos = 4)
print(result_no_group)

# Con agrupación, especificando variables y pesos
grouped_mtcars <- mtcars %>% group_by(cyl)
result_grouped <- matriz.covar2(grouped_mtcars, variable = c("mpg", "disp", "hp"), tipo = "cuasi")
print(result_grouped)

# Agrupar mtcars por 'cyl' y calcular la matriz de covarianzas para cada grupo
grouped_mtcars <- mtcars %>% group_by(cyl) |> matriz.covar2(variable=c(1,3,6),tipo = "cuasi")

# Imprimir resultados
lapply(result_grouped, print)  # Imprime cada matriz de covarianzas en la lista


matriz.covar2(mtcars,variable=c(1,3,6))
cov(mtcars[c(1,3,6)])


matriz.covar2(mtcars,variable=c(1,3,6),tipo="cuasi")
cov(mtcars[c(1,3,6)])*31/32

matriz.covar2(mtcars,variable=c(1,3,6),pesos=4)
cov.wt(mtcars[c(1,3,6)], wt = mtcars$hp,method="ML")$cov

mtcars |>
  group_by(cyl) |>
  matriz.covar2(c(1,3,6),4)

