check_min_obs <- function(m, n_min_obs = 5) {
  output_matrix <- m
  n <- nrow(output_matrix)
  
  # Empezamos desde la última fila y vamos hacia arriba
  i <- n
  while (i > 1) {
    # Si el valor en la fila actual es menor a n_min_obs
    if (output_matrix$Freq_esp[i] < n_min_obs) {
      acum_freq <- output_matrix$Freq_esp[i]  # Empezamos la acumulación en Frey_obs
      acum_suma <- output_matrix$Freq_obs[i]      # Empezamos la acumulación en Freq_obs
      j <- i - 1
      
      # Seguimos hacia arriba sumando hasta que la acumulación sea >= n_min_obs
      while (acum_freq < n_min_obs && j > 0) {
        acum_freq <- acum_freq + output_matrix$Freq_esp[j]  # Acumulamos en Frey_obs
        acum_suma <- acum_suma + output_matrix$Freq_obs[j]      # Acumulamos en SUMA
        j <- j - 1
      }
      
      # Actualizamos la fila donde terminó la acumulación
      output_matrix$Freq_esp[j + 1] <- acum_freq
      output_matrix$Freq_obs[j + 1] <- acum_suma
      
      # Eliminamos las filas que se sumaron (entre j + 2 e i)
      if ((j + 2) <= i) {
        output_matrix <- output_matrix[-((j + 2):i), ]
      }
      
      # Ajustamos el valor de i para continuar desde la fila donde se terminó la acumulación
      i <- j + 1
    } else {
      # Si la fila actual ya es >= n_min_obs, seguimos con la fila anterior
      i <- i - 1
    }
  }
  
  return(output_matrix)
}
