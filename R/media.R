#' @title Media (aritmética).
#'
#' @description Calcula la media aritmética.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @usage media(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la media (aritmética) de todas la variables seleccionadas en un \code{data.frame}. En caso contrario, devuelve únicamente la media de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' Si se obtiene la media (muestral) a partir de los datos brutos, como generalmente hacen los softwares:
#'
#' \if{html}{\figure{media.png}{options: width="80" alt="Figure: media.png"}}
#' \if{latex}{\figure{media.png}{options: width=3.5cm}}
#'
#' Si se desea obtener la media (muestral) a partir de una tabla estadística se utiliza la expresión:
#'
#' \if{html}{\figure{media2.png}{options: width="80" alt="Figure: media2.png"}}
#' \if{latex}{\figure{media2.png}{options: width=3.5cm}}
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la media poblacional:
#'
#' \if{html}{\figure{mediapob.png}{options: width="80" alt="Figure: mediapob.png"}}
#' \if{latex}{\figure{mediapob.png}{options: width=3cm}}
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
#' media1 <- media(startup[1])
#' media2 <- media(startup,variable=1)
#' media3 <- media(salarios2018,variable=7, pesos= 10)
#'
#' @importFrom stats na.omit
#'
#' @import dplyr
#'
#' @export
media <- function(x, variable = NULL, pesos = NULL){

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

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la media, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    media <- apply(x,2,mean,na.rm=TRUE)
    media <- as.data.frame(t(media))

  } else{

    media <- x %>%
      na.omit %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      dplyr::summarize(media = sum(variable2*pesos)/sum(pesos)) %>%
      as.data.frame()

    varnames <- varnames[1]


  }


  names(media) <- paste("media_",varnames,sep="")

  return(media)

}


