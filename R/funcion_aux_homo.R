check_min_obs_extended_cols <- function(obs_matrix, exp_matrix, n_min_exp = 5) {

  output_obs <- obs_matrix
  output_exp <- exp_matrix
  n_cols <- ncol(output_exp)
  
  # Empezamos desde la última columna de la matriz de frecuencias esperadas
  i <- n_cols
  while (i > 1) {
    # Verificamos si algún valor en la columna actual de la matriz de frecuencias esperadas es menor a 5
    if (any(output_exp[, i] < n_min_exp)) {
      # Acumulamos toda la columna actual en la columna anterior
      output_exp[, i - 1] <- output_exp[, i - 1] + output_exp[, i]
      output_obs[, i - 1] <- output_obs[, i - 1] + output_obs[, i]
      
      # Eliminamos la columna actual en ambas matrices
      output_exp <- output_exp[, -i]
      output_obs <- output_obs[, -i]
      
      # Ajustamos la cantidad de columnas
      i <- ncol(output_exp)
    } else {
      # Si ningún valor en la columna actual es menor a 5, pasamos a la columna anterior
      i <- i - 1
    }
  }
  
  return(list(observadas = output_obs, esperadas = output_exp))
}

