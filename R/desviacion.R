#' @title Desviación típica.
#'
#' @description Calcula la desviación típica.
#'
#' \if{html}{\figure{qrdispersion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
<<<<<<< HEAD
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
=======
#' \if{latex}{\figure{qrdispersion.png}{options: scale=.25}}
>>>>>>> 52a86955ca07a23550d2c1b29bbf59bea51f4d18
#'
#' @usage desviacion(x,
#'          variable = NULL,
#'          pesos = NULL,
#'          tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la desviación típica muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasi-desviación típica muestral.
#'
#' @return Esta función devuelve un objeto de la clase \code{data.frame}. Si \code{tipo="muestral"}, devuelve la desviación típica muestral. Si \code{tipo="cuasi"}, devuelve la cuasi-desviación típica muestral.
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
#' (1) La expresión de la de la desviación típica muestral es:
#'
#' \if{html}{\figure{desviacion.png}{options: width="40\%"}}
#' \if{latex}{\figure{desviacion.png}{options: width=5cm}}
#'
#' La desviación típica muestral así definida es el estimador máximo verosímil de la desviación típica de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasidesviacion.png}{options: width="40\%"}}
#' \if{latex}{\figure{cuasidesviacion.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-desviación típica muestral y es un estimador insesgado de la desviación típica poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la desviación típica poblacional:
#'
#' \if{html}{\figure{desviacionpob.png}{options: width="40\%"}}
#' \if{latex}{\figure{desviacionpob.png}{options: width=5cm}}
#'
#' @seealso \code{\link{media}}, \code{\link{varianza}}, \code{\link{coeficiente.variacion}}
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
#' desviacion1 <- desviacion(startup[1])
#' desviaciona2 <- desviacion(startup,variable=1)
#' desviacion3 <- desviacion(startup,variable=1, tipo="cuasi")
#'
#' @importFrom stats sd na.omit
#'
#' @export
desviacion <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral","cuasi")){

  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

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

      stop("Para calcular la desviaci\u00f3n t\u00edpica a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")

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


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[c(variable,pesos)]

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la desviaci\u00f3n t\u00edpica, alguna variable que has seleccionado no es cuantitativa")
  }


  if(is.null(pesos) & tipo == "muestral"){

    n <- nrow(x)
    factor = (n-1)/n

  } else{

    factor <- 1
  }

  if(is.null(pesos)){

    desviacion <- apply(x,2,sd,na.rm=TRUE)
    desviacion <- sqrt(factor) * desviacion
    desviacion <- as.data.frame(t(desviacion))


  } else{

    desviacion <- x %>%
      na.omit %>%
      rename(variable2 = varnames[1], pesos = varnames[2]) %>%
      mutate(media = as.numeric(media(x,variable=1,pesos=2)),
             sumatorio = (variable2-media)^2*pesos)

    varnames <- varnames[1]

    if(tipo == "muestral"){

      desviacion <- desviacion %>% summarize(desviacion = sqrt(sum(sumatorio)/sum(pesos)))

    } else{

      desviacion <- desviacion %>% summarize(desviacion = sqrt(sum(sumatorio)/(sum(pesos)-1)))

    }

    names(desviacion) <- paste("desviacion_",varnames[1],sep="")


  }

  names(desviacion) <- paste("desviacion_",varnames,sep="")

  return(desviacion)

}
