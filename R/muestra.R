#' @title Tamaño de la muestra.
#'
#' @description Calcula el tamaño muestral para estimar la media de una población normal o la proporcion p de una población.
#' @usage muestra(poblacion = c("normal","dicotomica"),
#'                error_estimacion = NULL,
#'                confianza = 0.95,
#'                irrestricto = FALSE)
#'
#' @section Detalles:
#' La expresión de la media es:
#' \eqn{\bar{x}=\frac{x_{i}*n_{i}}{n}}
#'
#' @param poblacion Texto, si es "normal" (por defecto) calcula el tamaño muestral que permita estimar la media de una población normal, si "dicotomica" para estimar la proporción p de una población.
#' @param error_estimacion Es un valor que establece el error de estimación. Es la semiamplitud (mitad de la precisión) del intervalo de confianza. Esta aproximación solo es válida en distribuciones simétricas (normal o t-student).
#' @param confianza Es un valor entre 0 y 1 que indica el nivel de confianza. Por defecto el valor es 0.95
#' @param irrestricto Es un valor lógico que indica si se considera un muestreo aleatorio simple (por defecto, FALSE) o sin reemplazamiento (TRUE).
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#'
#' @references
#' Esteban García, J. et al. (2005). Estadística descriptiva y nociones de probabilidad. Thomson.
#'
#'
#' @import
#'
#' @export
muestra <- function(poblacion = c("normal","dicotomica"),
                    error_estimacion = NULL,
                    confianza = 0.95,
                    irrestricto = FALSE){


  source("./R/desviacion.R")
  source("./R/media.R")

  tipo_poblacion <- c("normal","dicotomica")
  poblacion <- tolower(poblacion)

  poblacion <- tolower(poblacion)
  poblacion <- match.arg(poblacion)

  if(!poblacion %in% tipo_poblacion){
    print("La distribuci\u00f3n de la poblaci\u00f3n tiene que ser normal o dicot\u00f3mica")
    stop("Se ha detectado un error en la distribuci\u00f3n de la poblaci\u00f3n")

  }

  if(is.null(error_estimacion)){

    stop("Es necesario indicar el error de estimaci\u00f3n (semiamplitud del intervalo de confianza).")

  }

  if(confianza >= 0 & confianza <=1){

    confianza <- confianza
    alfa_2 <- (1-confianza)/2
    valor_critico <- qnorm(alfa_2,lower.tail=FALSE)

  } else{

    stop("El nivel de confianza debe fijarse entre 0 y 1")

  }


  if(poblacion == "normal"){

    varianza_pob <- as.numeric(readline('\u00bfLa varianza poblacional es conocida? \n 1. "S\u00ed" \n 2. "No" \n'))

    varianza_pob <- as.numeric(varianza_pob)

    if(varianza_pob == 1){

      varianza <- readline(prompt="Introduce el valor de la varianza poblacional: ")

    } else{

      var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

      if(var_muestra == "1"){

        print("Se utilizar\u00e1 la varianza muestral como estimador de la varianza poblacional")
        varianza <- readline(prompt="Introduce el valor de la varianza muestral: ")

        } else{

        print("Se utilizar\u00e1 la cuasi-varianza muestral como estimador de la varianza poblacional")
        n_piloto <- readline(prompt="Introduce el tama\u00f1o de la muestra piloto: ")
        n_piloto <- as.numeric(n_piloto)
        varianza <- readline(prompt="Introduce el valor de la cuasi-varianza muestral de la muestra piloto: ")

        valor_critico <- qt(alfa_2,n_piloto-1,lower.tail=FALSE)

      }

    }

    varianza <- as.numeric(varianza)

    if(isFALSE(irrestricto)){

      tamano <- valor_critico^2 * (varianza / error_estimacion^2)

      if(var_muestra == 1){ # normal, var desconocida y varianza muestral

        tamano <- 1 + valor_critico^2 * (varianza / error_estimacion^2)

      }

    } else{

      N <- readline(prompt = "Introduce el tama\u00f1o de la poblaci\u00f3n (N): ")
      N <- as.numeric(N)

      numerador <- N * valor_critico^2 * varianza
      denominador <- error_estimacion^2 * (N-1) + valor_critico^2 * varianza
      tamano <- numerador / denominador
    }

  } else{ #poblacion dicotomica

    p_mu <- as.numeric(readline('\u00bfQuieres aproximar la p poblacional por la proporci\u00f3n muestral? \n 1. "S\u00ed" \n 2. "No" \n'))
    p_mu <- as.numeric(p_mu)

    if(p_mu == 1){

    p <- readline(prompt = "Introduce el valor de la proporci\u00f3n muestral: ")
    p <- as.numeric(p)

    } else{

      print("Aproximaci\u00f3n adecuada cuando no se tiene informaci\u00f3n sobre la proporci\u00f3n. Se considerar\u00e1 p=q=0.5")

      p <- 0.5

    }

    if(isFALSE(irrestricto)){

      tamano <- valor_critico^2 *  (p * (1-p) / error_estimacion^2)


    } else{

      N <- readline(prompt = "Introduce el tama\u00f1o de la poblaci\u00f3n (N): ")
      N <- as.numeric(N)

      numerador <- (valor_critico^2 * p * (1-p)) / error_estimacion^2
      denominador <- 1 + (valor_critico^2 * p * (1-p))/(error_estimacion^2 * N)

      tamano <-  numerador / denominador

      }


    }


  tamano <- ceiling(tamano)
  tamano <- as.data.frame(tamano)
  names(tamano) <- "tama\u00f1o de la muestra"

  return(tamano)

}


