#' Método de impresión para objetos resumen_descriptivos
#'
#' @param x Objeto de clase resumen_descriptivos
#' @param ... Argumentos adicionales
#' @export
print.resumen <- function(x, ...) {
  print(format(round(x, 4), scientific = FALSE, nsmall = 4), quote = FALSE, ...)
}
