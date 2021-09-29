#' @title Intervalo confianza para la diferencia de dos proporciones.
#'
#' @description Calcula el intervalo de confianza de la diferencia de dos proporciones.
#' @usage ic.diferencia.proporciones(x,
#'                      variable = NULL,
#'                      introducir = FALSE,
#'                      confianza = 0.95)
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
#' Se obtiene el intervalo:
#'
#' \figure{ic_dif_proporciones.png}{options: width="65\%" heigth="65\%"}
#'
#' Nota: Las proporciones muestrales del error típico son sustituidas por sus estimaciones máximo-verosímiles (proporciones muestrales).
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
ic.diferencia.proporciones <- function(x,
                                       variable = NULL,
                                       introducir = FALSE,
                                       confianza = 0.95){


if(isFALSE(introducir)) {

  print("En tus datos tiene que haber una variable de agrupaci\u00f3n y una variable con \u00e9xitos(=1) y fracasos(=0)")

  if(is.null(variable)){

    if(length(x) == 2){

      x <- data.frame(x)

      agrupacion <- readline(prompt="Indica la posici\u00f3n (n\u00famero de columna) de la variable de agrupaci\u00f3n: ")
      agrupacion <- as.numeric(agrupacion)

      x <- x %>%
        select(all_of(agrupacion),everything())
      x <- na.omit(x)


    } else{

      stop("El conjunto de datos seleccionado no tiene 2 variables.")

    }

  } else if(length(x) > 2){

    agrupacion <- readline(prompt="Indica la posici\u00f3n (n\u00famero de columna) de la variable de agrupaci\u00f3n: ")
    agrupacion <- as.numeric(agrupacion)

    exito <- readline(prompt="Indica la posici\u00f3n (n\u00famero de columna) de la variable con \u00e9xitos (=1) y fracasos (=0): ")
    exito <- as.numeric(exito)

    variable <- c(agrupacion,exito)

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)
    x <- na.omit(x)


  } else{

    stop("Selecci\u00f3n err\u00f3nea de variables para calcular el IC")

  }


  if(!all(x[,2] == 0 | x[,2]==1)){

    print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9xito) y unos (presencia/\u00e9xito)")
    stop("Los valores en la muestra deben ser 0 y 1.")

  }

  if(!length(unique(x[,1]))==2){

    stop("La variable de agrupaci\u00f3n no tiene dos categor\u00edas")

  }

  df <- table(x)


  # tama\u00f1o de la muestra
  n <- apply(df,1,sum)
  n1 <- as.numeric(n[1])
  n2 <- as.numeric(n[2])

  # media muestral
  p_mu1 <- as.numeric(df[1,2]/n1)
  p_mu2 <- as.numeric(df[2,2]/n2)


} else{   # aqu\u00ed empieza introducir datos


  print("Primero vas a introducir los datos de la muestra 1 y a continuaci\u00fen introducir\u00e1s los datos de la muestra 2")
  print("Si los datos provienen de encuestas realizadas antes y despu\u00e9s de una determinada acci\u00f3n, introduce primero los datos de la encuesta realizada despu\u00e9s de dicha acci\u00f3n")

  n1 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 1: ")
  n1 <- as.numeric(n1)


  p_mu1 <- readline(prompt = "Introducir el valor de la proporci\u00f3n muestral 1: ")
  p_mu1 <- as.numeric(p_mu1)


  n2 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 2: ")
  n2 <- as.numeric(n2)

  p_mu2 <- readline(prompt = "Introducir el valor de la proporci\u00f3n muestral 2: ")
  p_mu2 <- as.numeric(p_mu2)


}

  if(confianza >= 0 & confianza <=1){

    confianza <- confianza
    alfa_2 <- (1-confianza)/2
    valor_critico <- qnorm(alfa_2,lower.tail=FALSE)

  } else{

    stop("El nivel de confianza debe fijarse entre 0 y 1")

  }


  error_tipico <- sqrt((p_mu1 * (1-p_mu1))/n1 + (p_mu2 * (1-p_mu2))/n2)

  limite_inf <- (p_mu1 - p_mu2) - valor_critico * error_tipico
  limite_sup <- (p_mu1 - p_mu2) + valor_critico * error_tipico

  if(limite_inf < limite_sup){

    limite_inferior <- limite_inf
    limite_superior <- limite_sup

  } else{

    limite_inferior <- limite_sup
    limite_superior <- limite_inf
  }


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}

