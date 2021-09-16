#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#' @usage resumen.descriptivos(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Olga Blasco-Blasco} (\email{olga.blasco@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Universidad de Valencia (España)
#'
#' @references
#' Esteban García, J. et al. (2005). Estadística descriptiva y nociones de probabilidad. Thomson.
#'
#' @import dplyr
#'
#' @export
resumen.descriptivos <- function(x, variable = NULL, pesos = NULL){

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

        stop("Seleccion errronea de variables")

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
    varnames <- varnames[variable]

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
    valor_forma <- as.data.frame(t(medidas.forma(x)))
    valor_moda <- as.data.frame(t(apply(x,2,moda)))


  } else{

    valor_media <- media(x,variable=1,pesos=2)
    names(valor_media)
    valor_cuartiles <- cuantiles(x,variable=1,pesos=2, cortes = c(0,0.25,0.5,0.75,1))
    valor_varianza <- varianza(x,variable=1,pesos=2)
    valor_desviacion <- desviacion(x,variable=1,pesos=2)
    valor_coef_variacion <- coeficiente.variacion(x,variable=1,pesos=2)
    valor_forma <- as.data.frame(t(medidas.forma(x,variable=1,pesos=2)))
    valor_moda <- as.data.frame(moda(x,variable=1,pesos=2))

    varnames <- varnames[1]

  }

  resumen <- data.table::rbindlist(list(valor_media,
                                        valor_cuartiles,
                                        valor_varianza,
                                        valor_desviacion,
                                        valor_coef_variacion,
                                        valor_forma,
                                        valor_moda),
                                   use.names = FALSE)

  resumen <- as.data.frame(resumen)
  names(resumen) <- varnames

  num_modas <-nrow(valor_moda)

  row.names(resumen) <- c("media","mínimo","cuartil 1","mediana","cuartil 3", "máximo","varianza","desviacion tipica",
                          "coef_variacion","asimetria","curtosis",paste("moda ",seq(1:num_modas),sep=""))

  return(resumen)

}

