#' @title Intervalo confianza para el coeficiente de correlación
#'
#' @description Calcula el intervalo de confianza para el coeficiente de correlación.
#'
#' @usage ic.correlacion(x,
#'                 variable = NULL,
#'                 introducir = FALSE,
#'                 confianza = 0.95)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
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
#' @details
#'
#' (1) El intervalo para
#'
#' \figure{ic_correlacion_1.png}{options: width="20\%" heigth="20\%"}
#'
#' (2) es:
#'
#' \figure{ic_correlacion_2.png}{options: width="80\%" heigth="80\%"}
#'
#' Igualando la expresión en (1) al extremo inferior de (2) y al extremo superior de (2) se obtendrá el intervalo para la correlación.
#'
#' @references
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
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
  correlacion <- correlacion(x)

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
