library(dplyr)

coeficiente.variacion.nuevo <- function(data, variable = NULL, pesos = NULL, tipo = "muestral") {
  # Validar y establecer el tipo de desviación
  tipo <- match.arg(tolower(tipo), choices = c("muestral", "cuasi"))

  # Si data es un vector numérico, transformarlo en un dataframe
  if (is.numeric(data) && is.null(variable) && is.null(pesos)) {
    data <- data.frame(variable = data)
    variable = "variable"
  } else if (is.numeric(data) && is.numeric(variable) && is.null(pesos)) {
    data <- data.frame(variable = data, pesos = variable)
    variable = "variable"
    pesos = "pesos"
  } else if (is.numeric(data) && is.numeric(variable) && is.numeric(pesos)) {
    stop("Ha introducido más de dos vectores")
  } else if (!is.data.frame(data)) {
    stop("El argumento data debe ser un dataframe o un vector numérico.")
  }

  # Ajustar variable y pesos si son introducidos como índices numéricos
  if (!is.null(variable) && is.numeric(variable)) {
    if(any(variable > length(names(data))) || any(variable < 1)){
      stop("Índice de variable fuera de rango.")
    }
    variable <- names(data)[variable]
  } else if (is.null(variable)) {
    # selecciona todas las columnas numéricas por defecto excluyendo columnas de agrupación
    group_cols <- if(is.grouped_df(data)) group_vars(data) else character(0)
    variable <- setdiff(names(data)[sapply(data, is.numeric)], group_cols)
  }

  if (!is.null(pesos) && is.numeric(pesos)) {
    if(pesos > length(names(data)) || pesos < 1){
      stop("Índice de pesos fuera de rango.")
    }
    pesos <- names(data)[pesos]
  }

  # Excluir la columna de pesos de las variables si es necesario
  if (!is.null(pesos)) {
    variable <- setdiff(variable, pesos)
  }

  # Calcular la desviación estándar y coeficiente de variación
  results_df <- data %>%
    summarise(across(all_of(variable),
                     ~ {
                       if (is.null(pesos)) {
                         sd_x <- desviacion.nueva(.x,tipo=tipo)
                         media_x <- media.nueva(.x)
                         sd_x / media_x
                       } else {
                         # Asegurar casos completos
                         datos_completos <- na.omit(data.frame(valores = .x, pesos = .data[[pesos]]))
                         # Desviación estándar ponderada
                         media_ponderada <- media.nueva(datos_completos$valores,datos_completos$pesos)
                         sd_ponderada <- desviacion.nueva(datos_completos$valores,datos_completos$pesos,tipo=tipo)
                         sd_ponderada / media_ponderada
                       }
                     },
                     .names = "coef_var_{.col}"),
              .groups = "drop")

  return(results_df)
}
