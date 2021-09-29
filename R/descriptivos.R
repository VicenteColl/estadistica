#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#' @usage resumen.descriptivos(x, variable = NULL, pesos = NULL, exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
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
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr
#'
#' @export
resumen.descriptivos <- function(x, variable = NULL, pesos = NULL, exportar = FALSE){

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

  row.names(resumen) <- c("media","m\u00ednimo","cuartil 1","mediana","cuartil 3", "m\u00e1ximo","varianza","desviacion tipica",
                          "coef_variacion","asimetria","curtosis",paste("moda ",seq(1:num_modas),sep=""))

  if (exportar) {
    filename <- paste("Resumen descriptivos basicos"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(resumen, row.names = TRUE, file = filename)
  }

  return(resumen)

}

