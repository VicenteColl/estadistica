.onAttach <- function(...) {
  packageStartupMessage("Este paquete esta pensado para servir de apoyo a la docencia de estadística descriptiva e inferencial.")
  packageStartupMessage("Por favor, observa que este paquete está en desarrollo.")
  suppressPackageStartupMessages(library(ggalt))
  }
