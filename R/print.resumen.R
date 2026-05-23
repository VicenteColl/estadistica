#' Método de impresión para objetos de clase "resumen"
#'
#' @param x Objeto de clase "resumen"
#' @param ... Argumentos adicionales
#'
#' @export
#' @method print resumen
print.resumen <- function(x, ...) {

  x <- as.data.frame(x)

  op <- options(scipen = 999)
  on.exit(options(op), add = TRUE)

  print.data.frame(
    x,
    row.names = TRUE,
    quote = FALSE
  )

  invisible(x)
}
