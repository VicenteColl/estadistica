#' @title Mediana.
#'
#' @description Calcula la mediana.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: scale=.25}}
#'
#' @usage mediana(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la mediana de todas la variables seleccionadas en un \code{data.frame}. En caso contrario, devuelve únicamente la mediana de la variable para la que se ha facilitado la distribución de frecuencias.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' La mediana se obtiene a partir de la siguiente regla de decisión:
#'
#' \if{html}{\figure{mediana.png}{options: width="80\%" alt="Figure: mediana.png"}}
#' \if{latex}{\figure{mediana.png}{options: scale=.8}}
#'
#' donde: Ni son las frecuencias acumuladas y n el tamaño de la muestra (o N si es la población).
#'
#' @seealso \code{\link{media}}, \code{\link{cuantiles}}
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
#' mediana1 <- mediana(startup[1])
#' mediana2 <- mediana(startup,variable=1)
#' mediana3 <- mediana(salarios2018,variable=7 , pesos=10 )
#'
#' @import dplyr
#'
#' @export
mediana <- function(x, variable = NULL, pesos = NULL){

  x <- data.frame(x)
  varnames <- names(x)

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

        stop("Seleccion erronea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es valido")
      }
    }

  }


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular la desviacion tipica a partir de la distribucion de frecuencias solo puedes seleccionar una variable y unos pesos")

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
    varnames <- names(x)

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la varianza, alguna variable que has seleccionado no es cuantitativa")
  }


  if(is.null(pesos)){

    mediana <- apply(x,2,mediana.int)
    names(mediana) <- paste("mediana_",varnames,sep="")

  } else{

    mediana <- x %>%
      na.omit %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      summarize(mediana = mediana.int(variable2,pesos)) %>%
      as.data.frame()

    names(mediana) <- paste("mediana_",varnames[1],sep="")


  }

  return(mediana)

}
