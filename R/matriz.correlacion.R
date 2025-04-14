#' @title Matriz de correlación.
#'
#' @description Obtiene la matriz de correlación (de Pearson) entre 2 o más variables cuantitativas.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcorrelacion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcorrelacion.png}{options: width=3cm}}
#'
#' @usage matriz.correlacion(x, variable = NULL, exportar = FALSE)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} solo tiene 2 variables (columnas), \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return La función devuelve la matriz de correlación lineal de las variables seleccionadas en un \code{data.frame}.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' Se obtiene la matriz de correlación muestral:
#'
#' \if{html}{\figure{matrizcorrelacion.png}{options: width="50\%" alt="Figure: matrizcorrelacion.png"}}
#' \if{latex}{\figure{matrizcorrelacion.png}{options: width=8cm}}
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la matriz de correlació poblacional:
#'
#' \if{html}{\figure{matrizcorrelacionpob.png}{options: width="55\%" alt="Figure: matrizcorrelacionpob.png"}}
#' \if{latex}{\figure{matrizcorrelacionpob.png}{options: width=8cm}}
#'
#' @seealso \code{\link{correlacion}}, \code{\link{covarianza}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' matriz_cor <- matriz.correlacion(startup)
#'
#'
#' @importFrom stats na.omit cor
#' @import dplyr
#'
#' @export
matriz.correlacion <- function(x, variable = NULL, exportar = FALSE){

  varnames <- as.character(names(x))
  x <- data.frame(x)
  names(x) <- varnames

  if(is.null(variable)){

    varcuan <-  names(x[unlist(lapply(x, is.numeric))])
    seleccion = match(varcuan,varnames)
    x <- x[seleccion]
    varnames <- varcuan

  } else{

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable

      } else{

        stop("Selecci\u00f3n err\u00f3nea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la matriz de correlaci\u00f3, alguna variable que has seleccionado no es cuantitativa")
  }


  matriz_cor <- cor(x) %>%
    as.matrix()
  colnames(matriz_cor) <- varnames
  row.names(matriz_cor) <- varnames

  # Exportar
  if (exportar) {

    filename <- paste0("Matriz_de_correlacion_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Matriz_correlacion")

    # nombres de fila a columna
    resumen_export <- cbind(' ' = row.names(matriz_cor), matriz_cor)
    row.names(resumen_export) <- NULL

    openxlsx::writeData(wb, "Matriz_correlacion", resumen_export)

    # formato numerico decimal en Excel
    addStyle(wb, "Matriz_correlacion",
             style = createStyle(numFmt = "0.0000"),
             rows = 2:(nrow(resumen_export)+1),
             cols = 2:(ncol(resumen_export)+1),
             gridExpand = TRUE)

    saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(matriz_cor)

}


