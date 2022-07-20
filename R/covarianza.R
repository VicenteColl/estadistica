#' @title Covarianza.
#'
#' @description Calcula la covarianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcovarianza.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcovarianza.png}{options: width=3cm}}
#'
#' @usage covarianza(x,
#' variable = NULL,
#' pesos = NULL,
#' tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x solo tiene 2 variables (columnas), el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la covarianza muestral (tipo = "muestral"). Si tipo = "cuasi", se calcula la cuasi-covarianza muestral.
#'
#' @return Esta función devuelve la covarianza en un objeto de la clase \code{vector}.
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
#' (1) La covarianza muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{covarianzamuestra.png}{options: width="50\%" alt="Figure: covarianzamuestra.png"}}
#' \if{latex}{\figure{covarianzamuestra.png}{options: width=6cm}}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la covarianza a partir de la expresión:
#'
#' \if{html}{\figure{covarianzacuasi.png}{options: width="50\%" alt="Figure: covarianzacuasi.png"}}
#' \if{latex}{\figure{covarianzacuasi.png}{options: width=6cm}}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la covarianza poblacional:
#'
#' \if{html}{\figure{covarianzapob.png}{options: width="50\%" alt="Figure: covarianzapob.png"}}
#' \if{latex}{\figure{covarianzapob.png}{options: width=6cm}}

#' @seealso \code{\link{varianza}}, \code{\link{desviacion}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom stats cov
#' @import dplyr
#'
#' @export
covarianza <- function(x,
                       variable = NULL,
                       pesos = NULL,
                       tipo = c("muestral","cuasi")){

  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  x <- data.frame(x)
  varnames <- names(x)


  if(is.null(variable)){

    if(length(x) == 2){

      varcuan <-  names(x[unlist(lapply(x, is.numeric))])
      seleccion = match(varcuan,varnames)
      x <- x[seleccion]
      varnames <- varcuan
    } else{
      warning("Para obtener la matriz de correlaci\u00f3n utilizar la funcion matriz.correlacion()")
      stop("El conjunto de datos seleccionado tiene mas de 2 variables.")
    }

  } else if(length(variable) == 2){

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

        stop("El nombre de la variable no es v\u00edlido")

      }

    }

  } else{

    warning("Para obtener la matriz de varianzas-covarianzas utilizar la funci\u00f3n matriz.covar()")
    stop("Para calcular la covarianza solo puedes seleccionar dos variables")
  }

  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    varnames <- varnames[variable]

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 3){

      stop("Para calcular la covarianza a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar dos variables y unos pesos")

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

    stop("No puede calcularse la covarianza, alguna variable que has seleccionado no es cuantitativa")

  }

  tipo_covarianza <- c("muestral","cuasi")

  if(!(tipo %in% tipo_covarianza)){

    stop("Indica si quieres calcular la covarianza muestral o la cuasi-covarianza")

  }


  if(is.null(pesos) & tipo == "muestral"){

    n <- nrow(x)
    factor = (n-1)/n

  } else{

    factor <- 1
  }

  if(is.null(pesos)){

    covarianza <- factor * cov(x[1],x[2], use ="everything") %>%
      as.numeric()

  } else{

    x <- x %>%
      na.omit %>%
      rename(variable1 = varnames[1], variable2 = varnames[2], pesos = varnames[3])

    media1 <- as.numeric(estadistica::media(x,variable=1,pesos=3))
    media2 <- as.numeric(estadistica::media(x,variable=2,pesos=3))

    covarianza <- x %>%
      dplyr::mutate(sumatorio = (variable1-media1)*(variable2-media2)*pesos)

    #varnames <- paste(varnames[1],"_",varnames[2],sep="")

    if(tipo == "muestral"){

      covarianza <- covarianza %>%
        summarize(covarianza = sum(sumatorio)/sum(pesos)) %>%
        as.numeric()

    } else{

      covarianza <- covarianza %>%
        summarize(varianza = sum(sumatorio)/(sum(pesos)-1)) %>%
        as.numeric()


    }


  }


  names(covarianza) <- paste("covarianza_",varnames[1],"_",varnames[2],sep="")
  row.names(covarianza) <- NULL

  return(covarianza)

}
