#' @title Cuantiles.
#'
#' @description Calcula los cuantiles.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcuantiles.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcuantiles.png}{options: width=3cm}}
#'
#' @usage cuantiles(x,
#'                  variable = NULL,
#'                  pesos = NULL,
#'                  cortes = c(0.25,0.5,0.75),
#'                  exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param cortes Vector con los puntos de corte a calcular. Por defecto se calcula el primer, segundo y tercer cuartil.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Si \code{pesos = NULL}, la función devuelve los cuantiles de todas las variables seleccionadas en un objeto de tipo \code{data.frame}. En caso contrario, devuelve los cuantiles de la variable para la que se ha facilitado la distribución de frecuencias.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' @details
#'
#' Los cuantiles se obtienen a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{cuantiles.png}{options: width="85\%" alt="Figure: cuantiles.png"}}
#' \if{latex}{\figure{cuantiles.png}{options: scale=.85}}
#'
#' Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' cuartiles: s=1,2,3 y k=4
#'
#' deciles: s= 1,2,...,9 y k=10
#'
#' percentiles: s=1,2,...,99 y k=100
#'
#' @seealso \code{\link{media}}, \code{\link{mediana}}
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
#' cuantiles1 <- cuantiles(startup[1])
#' cuantiles2 <- cuantiles(startup,variable=1,cortes=seq(0.1,0.9,0.1))
#' cuantiles3 <- cuantiles(salarios2018,variable=6,pesos=7 )
#'
#' @export
cuantiles <- function(x,
                      variable = NULL,
                      pesos = NULL,
                      cortes = c(0.25,0.5,0.75),
                      exportar = FALSE){

  # Capturar nombre original
  var_name <- deparse(substitute(x))

  # Convertir a data.frame y capturar nombres
  x <- as.data.frame(x)
  original_names <- names(x)

  # --- Seleccion de variable ---
  if(is.null(variable)){
    varcuan <- names(x)[sapply(x, is.numeric)]
    x <- x[varcuan, drop = FALSE]
    varnames <- varcuan
  } else {
    if(is.numeric(variable)){
      if(any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
      varnames <- names(x)[variable]
      x <- x[, variable, drop = FALSE]
    } else if(is.character(variable)){
      if(!all(variable %in% names(x))) stop("Nombre de variable no v\u00e1lido")
      varnames <- variable
      x <- x[, variable, drop = FALSE]
    } else {
      stop("El argumento 'variable' debe ser num\u00e9rico o car\u00e1cter")
    }
  }

  # --- Comprobar tipo de variable ---
  if(!all(sapply(x, is.numeric))) stop("No puede calcularse los cuantiles: variable no cuantitativa")

  cortes <- sort(cortes)

  # --- Calcular cuantiles ---
  if(is.null(pesos)){
    cuantiles_result <- apply(x, 2, function(col) {
      quantile(col, probs = cortes, na.rm = TRUE, names = FALSE)
    })
  } else {
    # funcion interna para cuantiles ponderados
    cuantiles_result <- .cuantiles.int(x[[1]], x[[2]], cortes)
  }

  cuantiles_result <- as.data.frame(cuantiles_result)
  names(cuantiles_result) <- paste0("cuantiles_", varnames)
  row.names(cuantiles_result) <- paste0(cortes*100, "%")

  # --- Exportar ---
  if(exportar){
    filename <- paste0("Cuantiles_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Cuantiles")
    resumen_export <- cbind(Cuantil = row.names(cuantiles_result), cuantiles_result)
    row.names(resumen_export) <- NULL
    writeData(wb, "Cuantiles", resumen_export)
    addStyle(wb, "Cuantiles", style = createStyle(numFmt = "0.0000"),
             rows = 2:(nrow(resumen_export)+1), cols = 2:(ncol(resumen_export)+1), gridExpand = TRUE)
    saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(cuantiles_result)
}
