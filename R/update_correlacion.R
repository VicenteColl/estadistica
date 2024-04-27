correlacion.nueva <- function(.data, variables = NULL, pesos = NULL) {
  if (!is.data.frame(.data)) {
    stop("El argumento .data debe ser un dataframe.")
  }

  # Validar y transformar variables
  if (is.null(variables) || length(variables) != 2) {
    stop("Debe especificar exactamente dos variables para calcular la correlación.")
  }

  if (is.numeric(variables)) {
    if (any(variables > ncol(.data)) || any(variables < 1)) {
      stop("Índice de 'variables' fuera de rango.")
    }
    variables <- names(.data)[variables]
  } else if (is.character(variables)) {
    if (!all(variables %in% names(.data))) {
      stop("Una o más de las variables especificadas no existen en el dataframe.")
    }
  } else {
    stop("Las variables deben ser especificadas por nombre o por índice numérico.")
  }

  # Validar y transformar pesos si son especificados
  if (!is.null(pesos)) {
    if (is.numeric(pesos)) {
      if (pesos > ncol(.data) || pesos < 1) {
        stop("Índice de 'pesos' fuera de rango.")
      }
      pesos <- names(.data)[pesos]
    } else if (is.character(pesos)) {
      if (!pesos %in% names(.data)) {
        stop("La variable de pesos especificada no existe en el dataframe.")
      }
    } else {
      stop("Los pesos deben ser especificados por nombre o por índice numérico.")
    }

    if (pesos %in% variables) {
      stop("Los pesos no pueden ser una de las variables de interés.")
    }
  }

  # Calcular la correlación usando summarise
  resultado <- .data %>%
    dplyr::summarise(correlacion = {
      data_subset <- cur_data()  # Obtener el subconjunto de datos actual
      if (is.null(pesos)) {
        cor_var = cor(data_subset[[variables[1]]], data_subset[[variables[2]]], use = "complete.obs")
      } else {
        cov = covarianza.nueva(data_subset, variable = variables, pesos = pesos)
        sd_x = desviacion.nueva(data_subset, variable = variables[1], pesos = pesos)
        sd_y = desviacion.nueva(data_subset, variable = variables[2], pesos = pesos)
        cor_var = cov / (sd_x * sd_y)
      }
      cor_var
    }, .groups = "drop")  # Manejar el agrupamiento

  names(resultado) <- paste("correlacion", variables[1], variables[2], sep = "_")
  return(resultado)
}


mtcars |>
  group_by(cyl) |>
  correlacion.nueva(c("mpg","wt"))

mtcars |>
  group_by(cyl) |>
  summarize(corr=cor(mpg,wt))

mtcars |>
  group_by(cyl) |>
  correlacion.nueva(c(1,6))

cor(mtcars$mpg,mtcars$wt)
correlacion.nueva(mtcars,c("mpg","wt"))
correlacion.nueva(mtcars,c(1,6))
estadistica::correlacion(mtcars,c("mpg","wt"))
estadistica::correlacion(mtcars,c(1,6))


correlacion.nueva(mtcars,c("mpg","wt"),pesos="disp")
correlacion.nueva(mtcars,c(1,6),3)
estadistica::correlacion(mtcars,c("mpg","wt"),"disp")
estadistica::correlacion(mtcars,c(1,6),3)
