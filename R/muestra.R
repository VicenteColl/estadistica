#' @title Tamaño de la muestra.
#'
#' @description Calcula el tamaño muestral para estimar la media de una población normal o la proporcion p de una población.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrmuestra1.png}{options: width="25\%" alt="Figure: qrmuestra1.png"}}
#' \if{latex}{\figure{qrmuestra1.png}{options: width=3cm}}
#'
#' \if{html}{\figure{qrmuestra2.png}{options: width="25\%" alt="Figure: qrmuestra2.png"}}
#' \if{latex}{\figure{qrmuestra2.png}{options: width=3cm}}
#'
#' @usage muestra(poblacion = c("normal","dicotomica"),
#'                error_estimacion = NULL,
#'                confianza = 0.95,
#'                irrestricto = FALSE)
#'
#' @param poblacion Texto, si \code{poblacion = "normal"} (por defecto), calcula el tamaño muestral que permita estimar la media de una población normal. Si \code{poblacion = "dicotomica"}, para estimar la proporción p de una población.
#' @param error_estimacion Es un valor que establece el error de estimación. Es la semiamplitud (mitad de la precisión) del intervalo de confianza. Esta aproximación solo es válida en distribuciones simétricas (normal o t-student).
#' @param confianza Es un valor entre 0 y 1 que indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento).
#' @param irrestricto Es un valor lógico que indica si se considera un muestreo aleatorio simple (por defecto, \code{irrestricto = FALSE}) o sin reemplazamiento (\code{irrestricto = TRUE}).
#'
#' @return Devuelve el tamaño de la muesta en un objeto de tipo \code{data.frame}.
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
#' (1) El tamaño muestral para estimar la media poblacional se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{tamanomedia.png}{options: width="30\%" alt="Figure: tamanomedia.png"}}
#' \if{latex}{\figure{tamanomedia.png}{options: width=4cm}}
#'
#' y si el muestreo es irrestricto:
#'
#' \if{html}{\figure{tamanomediairrestricto.png}{options: width="60\%" alt="tamanomediairrestricto.png"}}
#' \if{latex}{\figure{tamanomediairrestricto.png}{options: width=8cm}}
#'
#' Nota: si la varianza poblacional no es conocida puede estimarse a través de la varianza (o cuasi-varianza) muestral.
#'
#' (2) El tamaño muestral para estimar la proporción de una característica se obtiene a partir de la expresión:
#'
#' \if{html}{\figure{tamanoproporcion.png}{options: width="35\%" alt="Figure: tamanoproporcion.png"}}
#' \if{latex}{\figure{tamanoproporcion.png}{options: width=4.5cm}}
#'
#' y si el muestreo es irrectricto:
#'
#' \if{html}{\figure{tamanoproporcionirrestricto.png}{options: width="65\%" alt="Figure: tamanoproporcionirrestricto.png"}}
#' \if{latex}{\figure{tamanoproporcionirrestricto.png}{options: width=10cm}}
#'
#' Nota: puede estimarse la proporción poblacional por la proporción muestral o, en caso
#' de no disponer de información, suponer el caso más desfavorable: p=q=0.5

#' @note
#' En el caso del tamaño muestral para la media: si la varianza poblacional no es conocida puede estimarse con la varianza muestral (o cuasivarianza muestral).
#' En el caso del tamaño muestral para la proporción: si la proporción poblacional no es conocida, puede estimarse por la proporción muestral o considerar el caso más desfavorable (p=q=0.5)
#'
#' @references
#' Casas José M. (1997) Inferencia estadística. Editoral: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' @importFrom stats qt pt qnorm pnorm
#'
#' @import dplyr
#'
#' @export
muestra <- function(poblacion = c("normal","dicotomica"),
                    error_estimacion = NULL,
                    confianza = 0.95,
                    irrestricto = FALSE){



  poblacion <- tolower(poblacion)
  poblacion <- match.arg(poblacion)

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

    if(varianza_pob == 1){

      varianza <- readline(prompt="Introduce el valor de la varianza poblacional: ")

    } else{

      var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

      if(var_muestra == 1){

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

      if(varianza_pob != 1){ # normal, var desconocida y varianza muestral

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


