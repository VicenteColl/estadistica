#' @title Moda.
#'
#' @description Calcula la moda.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @usage moda(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la moda de todas la variables seleccionadas en un \code{data.frame}. En caso contrario, devuelve únicamente la moda de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr
#'
#' @export
moda <- function(x, variable = NULL, pesos = NULL){

  if(class(x) %in% c("numeric","factor","integer","logical")){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)
  names(x) <- varnames

  if(is.null(variable)){

    varcuan <- x %>% select_if(function(col) is.numeric(col) |
                                 is.integer(col) |
                                 is.factor(col) |
                                 is.logical(col)) %>%
      names()

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

      stop("Para calcular la media a partir de la distribuci\u00fn de frecuencias solo puedes seleccionar una variable y unos pesos")

    }

    if(is.numeric(pesos)){

      pesos <- pesos

    }


    if(is.character(pesos)){

      if(pesos %in% varnames){
        pesos = match(pesos,varnames)
      } else {
        stop("El nombre de los pesos no es v\u00e1lido")
      }
    }

    if(pesos == variable){

      stop("Has seleccionado la misma columna del dataframe para la variable y los pesos")

    }


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[c(variable,pesos)]

  }

  clase <- sapply(x, class)


  if (!all(clase %in% c("numeric","integer","factor","logic"))) {
    stop("No se puede calcular la moda, comprueba el tipo de variable seleccionada")
  }

  if(is.null(pesos)){

    moda_vacio <- vector("list",length=length(x))

    for(i in 1:length(x)){

      x_moda <- x[i] %>%
        na.omit

      valor_moda <- table(x_moda)
      valor_moda <- names(valor_moda)[which(valor_moda==max(valor_moda))]
      moda <- as.numeric(valor_moda)

        if(length(moda)==nrow(x_moda)){
          warning("Esta variable no tiene moda, todos los valores tienen la misma frecuencia")
          moda <- NA
        }

     moda_vacio[[i]] <- moda

    }

    max_long <-  max(lengths(moda_vacio))
    moda <- sapply(moda_vacio, "[", seq_len(max_long))

    if(is.vector(moda)){
      moda <- matrix(moda,nrow=1,ncol=length(moda)) %>%
        as.data.frame()
    } else{
      moda <- moda %>% as.data.frame()
    }

    names(moda) <- varnames

  } else{

       moda <- x %>%
        na.omit %>%
        rename(variable2 = varnames[1], pesos = varnames[2]) %>%
        group_by(variable2) %>%
        summarize(frecuencia = sum(pesos), .groups = 'drop') %>%
        arrange(desc(frecuencia)) %>%
        as.data.frame()

      valores_distintos <- unique(moda$variable2)

      moda <- moda %>%
        summarize(moda = variable2[which(frecuencia == max(frecuencia))]) %>%
        as.data.frame()

      if(nrow(moda) == length(valores_distintos)){
        warning("Esta variable no tiene moda, todos los valores tienen la misma frecuencia")
        moda <- NA
      }

      names(moda) <- paste("moda_",varnames[1],sep="")

  }

  return(moda)

}

