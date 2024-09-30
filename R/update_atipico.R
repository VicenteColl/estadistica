#' @import openxlsx dplyr ggplot2


atipicos <- function(data, variable, metodo = c("zscore", "iqr"), pesos = NULL, tipo = c("muestral", "cuasi"), exportar = FALSE) {
  # Validar y establecer el tipo
  tipo <- match.arg(tolower(tipo), choices = c("muestral", "cuasi"))

  # Validar y establecer el método
  metodo <- match.arg(tolower(metodo), choices = c("zscore", "iqr"))

  # Ajustar el nombre de la variable y validar la entrada
  if (is.null(variable) || !is.character(variable) || !all(variable %in% names(data))) {
    stop("El argumento 'variable' debe ser un nombre de columna válido.")
  }

  # Función auxiliar para calcular y graficar atípicos
  compute_outliers <- function(subdata, variable, metodo, media, sd, Q1, Q3, IQR) {
    if (metodo == "zscore") {
      z_scores <- (subdata[[variable]] - media) / sd
      outliers <- abs(z_scores) > 3
      subdata$outliers <- outliers
      return(subdata)
    } else if (metodo == "iqr") {
      outliers <- subdata[[variable]] < (Q1 - 1.5 * IQR) | subdata[[variable]] > (Q3 + 1.5 * IQR)
      subdata$outliers <- outliers
      return(subdata)
    }
  }

  results <- data %>%
    group_by(across(-all_of(variable))) %>%
    group_modify(~ {
      valores <- .x[[variable]]
      if (metodo == "zscore") {
        media <- mean(valores, na.rm = TRUE)
        sd <- sd(valores, na.rm = TRUE)
        return(compute_outliers(.x, variable, metodo, media, sd, NA, NA, NA))
      } else if (metodo == "iqr") {
        Q1 <- quantile(valores, 0.25, na.rm = TRUE)
        Q3 <- quantile(valores, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        return(compute_outliers(.x, variable, metodo, NA, NA, Q1, Q3, IQR))
      }
    }) %>%
    ungroup()

  # Crear gráficos por grupo si necesario
  if (metodo == "zscore") {
    p <- ggplot(results, aes_string(x = variable)) +
      geom_histogram(aes(y = ..density..), binwidth = sd / 10, fill = "grey", color = "black") +
      facet_wrap(~ group) +
      geom_vline(aes(xintercept = media - 3 * sd, color = "red"), linetype = "dashed") +
      geom_vline(aes(xintercept = media + 3 * sd, color = "red"), linetype = "dashed") +
      ggtitle("Histograma con Z-scores por grupo")
  } else {
    p <- ggplot(results, aes_string(x = "", y = variable)) +
      geom_boxplot() +
      facet_wrap(~ group) +
      ggtitle("Diagrama de caja y bigotes por grupo")
  }

  if (exportar) {
    write.xlsx(results, file = paste0("outliers_", variable, "_grouped.xlsx"))
  }

  print(p)  # Mostrar el gráfico
  return(results)
}
