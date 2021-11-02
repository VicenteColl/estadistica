#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#' @usage resumen.descriptivos(x,
#'                             variable = NULL,
#'                             pesos = NULL,
#'                             exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Esta función devuelve los principales estadísticos descriptivos muestrales en un objeto de tipo \code{data.frame}. Los descriptivos que se obtienen son: media, mínimo, cuartil 1, mediana, cuartil 3, máximo, varianza muestral, desviación típica muestral, coeficiente de variación, recorrido inter-cuartílico, asimetría, curtosis y moda.
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' descriptivos <- resumen.descriptivos(startup)
#'
#' @import dplyr
#'
#' @export
resumen.descriptivos <- function(x, variable = NULL, pesos = NULL, exportar = FALSE){

  old <- options()
  on.exit(options(old))

  options(scipen = 999)

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    x <- x

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

      stop("Para calcular los descriptivos a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")

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
    stop("No puede calcularse la varianza, alguna variable que has seleccionado no es cuantitativa")
  }


  if(is.null(pesos)){

    valor_media <- media(x)
    valor_cuartiles <- cuantiles(x, cortes = c(0,0.25,0.5,0.75,1))
    valor_varianza <- varianza(x)
    valor_desviacion <- desviacion(x)
    valor_coef_variacion <- coeficiente.variacion(x)
    ric <- cuantiles(x, cortes = 0.75) - cuantiles(x, cortes = 0.25)
    valor_forma <- as.data.frame(t(medidas.forma(x)))
    valor_moda <- as.data.frame(t(apply(x,2,moda)))


  } else{

    valor_media <- media(x,variable=1,pesos=2)
    names(valor_media)
    valor_cuartiles <- cuantiles(x,variable=1,pesos=2, cortes = c(0,0.25,0.5,0.75,1))
    valor_varianza <- varianza(x,variable=1,pesos=2)
    valor_desviacion <- desviacion(x,variable=1,pesos=2)
    valor_coef_variacion <- coeficiente.variacion(x,variable=1,pesos=2)
    ric <- cuantiles(x,variable=1,pesos=2, cortes = 0.75) - cuantiles(x,variable=1,pesos=2, cortes = 0.25)
    valor_forma <- as.data.frame(t(medidas.forma(x,variable=1,pesos=2)))
    valor_moda <- as.data.frame(moda(x,variable=1,pesos=2))

    varnames <- varnames[1]

  }

  resumen <- data.table::rbindlist(list(valor_media,
                                        valor_cuartiles,
                                        valor_varianza,
                                        valor_desviacion,
                                        valor_coef_variacion,
                                        ric,
                                        valor_forma,
                                        valor_moda),
                                   use.names = FALSE)

  resumen <- as.data.frame(resumen)
  names(resumen) <- varnames

  num_modas <-nrow(valor_moda)

  row.names(resumen) <- c("media","m\u00ednimo","cuartil 1","mediana","cuartil 3", "m\u00e1ximo","varianza","desviaci\u00f3n t\u00edpica",
                          "coef.variaci\u00f3n","RIC","asimetr\u00eda","curtosis",paste("moda ",seq(1:num_modas),sep=""))

  if (exportar) {
    filename <- paste("Resumen descriptivos basicos"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(resumen, row.names = TRUE, file = filename)
  }

  return(resumen)

}

