#' Método de impresión para objetos de clase "resumen"
#'
#' @param x Objeto de clase "resumen"
#' @param ... Argumentos adicionales
#' @export
print.resumen <- function(x, ...) {
  # Mostrar sin notacion cientifica, con 4 decimales
  print(format(round(x, 4), scientific = FALSE, nsmall = 4), quote = FALSE, ...)
}
