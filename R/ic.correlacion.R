#' @title Intervalo confianza para el coeficiente de correlación
#'
#' @description Calcula el intervalo de confianza para el coeficiente de correlación.
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento)
#'
#' @return Devuelve el intervalo de confianza de la correlación lineal en un objeto de tipo \code{data.frame}
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
#' (1) El intervalo para
#'
#' \if{html}{\figure{iccorrelacion1.png}{options: width="20\%" alt="iccorrelacion1.png"}}
#' \if{latex}{\figure{iccorrelacion1.png}{options: width=4cm}}
#'
#' (2) es:
#'
#' \if{html}{\figure{iccorrelacion2.png}{options: width="80\%" alt="Figure: iccorrelacion2.png"}}
#' \if{latex}{\figure{iccorrelacion2.png}{options: width=15cm}}
#'
#' Igualando la expresión en (1) al extremo inferior de (2) y al extremo superior de (2) se obtendrá el intervalo para la correlación.
#'
#' @references
#' Casas José M. (1997) Inferencia estadística. Editorial: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' @importFrom stats pnorm qnorm pt qt na.omit
#' @import dplyr
#'
#' @export
ic.correlacion <- function(x,
                           variable = NULL,
                           introducir = FALSE,
                           confianza = 0.95){

print("Calcula el intervalo de confianza de la correlaci\u00f3n de dos poblaciones con distribuci\u00f3n conjuntamente normal")

if(confianza >= 0 & confianza <=1){

  confianza <- confianza
  alfa_2 <- (1-confianza)/2
  valor_critico <- qnorm(alfa_2,lower.tail = FALSE)

} else{

  stop("El nivel de confianza debe fijarse entre 0 y 1")

}


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){
      x <- x
    } else{
      warning("Para calcular el intervalo de confianza hay que seleccionar 2 variables")
      stop("El conjunto de datos seleccionado no tiene la dimensi\u00f3n adecuada")
    }
  } else{

    if(length(variable) == 2){
        if(is.numeric(variable)){
          if(all(variable <= length(x))){
            variable <- variable
          } else{
            stop("Seleccion erronea de variable")
          }
        }
      if(is.character(variable)){
        if(all(variable %in% varnames)){
          variable = match(variable,varnames)
          } else {
            stop("El nombre de la variable no es v\u00e1lido")
          }
        }

      x <- x[,variable] %>% as.data.frame()
      names(x) <- varnames[variable]

      } else{
        warning("Para calcular el intervalo de confianza de la correlaci\u00f3 hay que seleccionar dos variables")
        stop("El conjunto de datos seleccionado parece ser no v\u00e1lido")
      }
  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))){
    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")
  }

  # tama\u00f1o de la muestra
  x <- na.omit(x)
  n <- nrow(x)
  correlacion <- as.numeric(correlacion(x))

} else{   # aqu\u00ed empieza introducir datos

  print("A continuaci\u00f3n, vas a introducir los datos muestrales.")

  n <- readline(prompt = "Introducir el tama\u00f3o de la muestra : ")
  n <- as.numeric(n)

  correlacion <- readline(prompt = "Introducir el valor del coeficiente de correlaci\u00f3n muestral: ")
  correlacion <- as.numeric(correlacion)

  if((correlacion < -1 | correlacion > 1)){

    stop("El coeficiente de correlaci\u00f3n debe estar comprendido entre -1 y 1.")

  }


}

  limite_inferior <- 0.5 * log((1+correlacion)/(1-correlacion)) - valor_critico * sqrt(1/(n - 3))
  limite_superior <- 0.5 * log((1+correlacion)/(1-correlacion)) + valor_critico * sqrt(1/(n - 3))

  limite_inferior <- (exp(2*limite_inferior)-1)/(1+exp(2*limite_inferior))
  limite_superior <- (exp(2*limite_superior)-1)/(1+exp(2*limite_superior))


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}
