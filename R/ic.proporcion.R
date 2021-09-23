#' @title Intervalo confinanza de una proporción.
#'
#' @description Calcula el intervalo de confianza de una proproción.
#' @usage ic.proporcion(x,
#'                      variable = NULL,
#'                      introducir = FALSE,
#'                      p_muestral = TRUE,
#'                      irrestricto = FALSE,
#'                      confianza = 0.95)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param p_muestral Es un valor lógico. Indica si se hace uso de la proporción muestral para estimar el intervalo de confianza (por defecto, p_muestral = TRUE). si p_muestral = FALSE, se considera p=q=0.5 (situación más desfavorable).
#' @param irrestricto Es un valor lógico. Por defecto, irrectricto = FALSE. si se considera un muestreo irrectricto (extracción sin reemplazamiento), cambiar el argumento a irrestricto = TRUE.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
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
ic.proporcion <- function(x,
                          variable = NULL,
                          introducir = FALSE,
                          p_muestral = TRUE,
                          irrestricto = FALSE,
                          confianza = 0.95){


  print("Intervalo de confianza de una proporción. El tamaño de la muestra es grande.")

  if(confianza >= 0 & confianza <=1){

    confianza <- confianza
    alfa2 <- (1- confianza)/2
    valor_critico <- qnorm(alfa2,lower.tail = F)

  } else{

    stop("El nivel de confianza debe fijarse entre 0 y 1")

  }


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 1){

      x <- x

    } else{

      warning("Para calcular el intervalo de confianza hay que seleccionar una variable")
      stop("El conjunto de datos seleccionado tiene mas de 1 variables.")

    }

  } else if(length(variable) == 1){

    if(is.numeric(variable)){

      if(variable <= length(x)){

        variable <- variable

      } else{

        stop("Seleccion erronea de variable")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){

        variable = match(variable,varnames)

      } else {

        stop("El nombre de la variable no es valido")

      }

    }

    x <- x[,variable] %>% as.data.frame()
    names(x) <- varnames[variable]

  } else{

    warning("Para calcular el intervalo de confianza hay que seleccionar una variable")
    stop("El conjunto de datos seleccionado tiene mas de 1 variables.")

  }

  if(!all(x == 0 | x==1)){

    print("Aplica a tus datos la condicion que debe cumplir la poblacion para transfomar los datos en ceros (ausencia/no exito) y unos (presencia/exito)")
    stop("Los valores en la muestra deben ser 0 y 1.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tamaño de la muestra
  n <- nrow(x)

  if(n < 30){
    stop("El tamaño de la muestra es pequeña, la aproximación a la normal no es buena.")
  }

  # media muestral

  if(isTRUE(p_muestral)){
    p_mu <- sum(x,na.rm=TRUE)/n
  } else{
    p_mu = 0.5
  }


} else{   # aquí empieza introducir datos

  n <- readline(prompt = "Introducir el tamaño de la muestra: ")
  n <- as.numeric(n)

  if(n < 30){
    stop("El tamaño de la muestra es pequeña, la aproximación a la normal no es buena.")
  }

  if(isTRUE(p_muestral)){

    p_mu <- readline(prompt = "Introducir el valor de la proporcion muestral: ")
    p_mu <- as.numeric(p_mu)

  } else{
    p_mu <- 0.5
  }


}


  if(isFALSE(irrestricto)){

    aproximacion <- as.numeric(readline('¿Quieres aproximar el valor de p por la proporcion muestral? \n 1. "Sí" \n 2. "No" \n'))
    if(aproximacion == 1){

      print("Como n es suficientemente grande, se aproxima el valor de p poblacional por su estimacion puntual (p muestral)")

      error_tipico <- sqrt((p_mu * (1-p_mu))/n)
      limite_inferior <- p_mu - valor_critico * error_tipico
      limite_superior <- p_mu + valor_critico * error_tipico

    } else{

      print("Este criterio no tiene en cuenta el tamaño de la muestra. Se obtendrá el intervalo de p a partir del cálculo de probabilidad del estadístico")
      print("El intervalo obtenido no es simétrico respecto a la proporción muestral")
      x <- n + valor_critico^2
      y <- -(2 * p_mu * n + valor_critico^2)
      z <- p_mu^2 * n

      limite_inf <- (-y + sqrt((y^2) - (4 * x * z)))/(2 * x)
      limite_sup <- (-y - sqrt((y^2) - (4 * x * z)))/(2 * x)

      if(limite_inf <= limite_sup){

        limite_inferior <- limite_inf
        limite_superior <- limite_sup

      } else{

        limite_inferior <- limite_sup
        limite_superior <- limite_inf

      }

    }

  } else{

    print("Intervalo para la proporcion adecuado si el muestreo es sin reemplazamiento y la poblacion es finita")

    N <- readline(prompt= "Introduce el tamaño (N) de la poblacion: ")
    N <- as.numeric(N)

    factor <- sqrt((N-n)/(N-1))
    error_tipico <- sqrt((p_mu * (1-p_mu))/n)
    limite_inferior <- p_mu - valor_critico * error_tipico * factor
    limite_superior <- p_mu + valor_critico * error_tipico * factor

  }


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}

