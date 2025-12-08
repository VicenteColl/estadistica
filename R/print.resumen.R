#' Método de impresión para objetos de clase "resumen"
#'
#' @param x Objeto de clase "resumen"
#' @param ... Argumentos adicionales
#' @method print resumen
#' @keywords internal
print.resumen <- function(x, ...) {
  x <- as.data.frame(x)
  print(format(round(x, 4), scientific = FALSE, nsmall = 4),
        quote = FALSE, ...)
}
