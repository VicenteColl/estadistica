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
cuantiles <- function(x, variable = NULL, pesos = NULL,
                      cortes = c(0.25,0.5,0.75),
                      exportar = FALSE){

  # Capturar el nombre original si es un vector
  var_name <- deparse(substitute(x))

  # Manejo de nombres para diferentes tipos de entrada
  if (is.data.frame(x) || is.list(x)) {
    # Para data.frames/listas
    original_names <- names(x)
    x <- as.data.frame(x)

    if (is.null(variable)) {
      varnames <- names(x)[sapply(x, is.numeric)]
    } else {
      if (is.numeric(variable)) {
        varnames <- names(x)[variable]
      } else {
        varnames <- variable
      }
    }
  } else {
    # Para vectores
    if (grepl("\\$", var_name)) {
      # Si es de tipo dataframe$columna
      varnames <- sub(".*\\$", "", var_name)
    } else {
      # Si es un vector simple
      varnames <- "variable"
    }
    x <- data.frame(x)
    names(x) <- varnames
    original_names <- varnames
  }

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

  }


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular los cuantiles a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")

    }

    if(is.numeric(pesos)){

      pesos <- pesos

    }

    if(is.character(pesos)){

      if(pesos %in% varnames){
        pesos = match(pesos,varnames)
      } else {
        stop("El nombre de los pesos no es valido")
      }
    }


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la media, alguna variable que has seleccionado no es cuantitativa")
  }

  cortes <- sort(cortes)

  if(is.null(pesos)){

    cuantiles <- apply(x,2,.cuantiles.int,cortes=cortes)



  } else{

    x <- x
    cuantiles <- .cuantiles.int(x[1],x[2],cortes)

  }

  cuantiles <- as.data.frame(cuantiles)
  names(cuantiles) <- paste("cuantiles_",varnames,sep="")
  row.names(cuantiles) <- paste(cortes*100,"%",sep="")

  # Exportar
  if (exportar) {

    filename <- paste0("Cuantiles_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Cuantiles")

    # nombres de fila a columna
    resumen_export <- cbind(Cuantil = row.names(cuantiles), cuantiles)
    row.names(resumen_export) <- NULL

    writeData(wb, "Cuantiles", resumen_export)

    # formato numerico decimal en Excel
    addStyle(wb, "Cuantiles",
             style = createStyle(numFmt = "0.0000"),
             rows = 2:(nrow(resumen_export)+1),
             cols = 2:(ncol(resumen_export)+1),
             gridExpand = TRUE)

    saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(cuantiles)

}
