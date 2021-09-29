#' @title Intervalo confianza para la varianza.
#'
#' @description Calcula el intervalo de confianza de la varianza poblacional.
#' @usage ic.varianza(x,
#'            variable = NULL,
#'            introducir = FALSE,
#'            media_poblacion = c("desconocida","conocida"),
#'            confianza = 0.95)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param media_poblacion Es un carácter. Indica si la media de la población es desconocida (por defecto, media_poblacion = "desconocida") o conocida (en este caso, cambiar media_poblacion = "conocida").
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
#' (1) Si la media poblacional es conocida:
#'
#'  \figure{ic_varianza_med_con.png}{options: width="50\%" heigth="50\%"}
#'
#' (2) Si la media poblacional es desconocida.
#'
#' Con la varianza muestral:
#'
#' \figure{ic_varianza_med_desc_muestra.png}{options: width="30\%" heigth="30\%"}
#'
#' Con la cuasivarianza muestral:
#'
#' \figure{ic_varianza_med_desc_cuasi.png}{options: width="40\%" heigth="40\%"}
#'
#' Nota: En todos los casos se obtiene el valor de la chi-dos con n grados de libertad que deja a su derecha una probabilidad de alfa y 1-alfa.
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
ic.varianza <- function(x,
                        variable = NULL,
                        introducir = FALSE,
                        media_poblacion = c("desconocida","conocida"),
                        confianza = 0.95){

  media_poblacion <- tolower(media_poblacion)
  media_poblacion <- match.arg(media_poblacion)

if(confianza >= 0 & confianza <=1){

  confianza <- confianza
  alfa_2 <- (1-confianza)/2

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
      stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

    }

  } else if(length(variable) == 1){

    if(is.numeric(variable)){

      if(variable <= length(x)){

        variable <- variable

      } else{

        stop("Selecci\u00f3n err\u00f3nea de variable")

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

    warning("Para calcular el intervalo de confianza hay que seleccionar una variable")
    stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra y grados libertad
  n <- nrow(x)
  gl <- n-1

  if(media_poblacion == "desconocida"){

    varianza_muestral <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(varianza_muestral == 1){

      var_mu <- as.numeric(varianza(x))

    } else{

      var_mu <- as.numeric(varianza(x, tipo = "cuasi"))
      n <- n-1

      print("Este es el intervalo de confianza que generalmente calculan los softwares (SPSS, Excel, Stata, ...)")

    }

  } else{

    print("La media poblacional no suele conocerse, este supuesto es te\u00f3rico")

    media <- readline(prompt = "Introducir el valor de la media poblacional: ")
    media <- as.numeric(media)

    sumatorio <- sum((x - media)^2)

  }

} else{   # aquí empieza introducir datos

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n <- as.numeric(n)
  gl <- n-1

  if(media_poblacion == "desconocida"){

    varianza_muestral <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(varianza_muestral == 1){

      var_mu <- readline("Introduce el valor de la varianza muestral: ")
      var_mu <- as.numeric(var_mu)
      n <- n

    } else{

      var_mu <- readline("Introduce el valor de la cuasivarianza muestral: ")
      var_mu <- as.numeric(var_mu)
      n <- n-1

      print("Este es el intervalo de confianza que generalmente calculan los softwares (SPSS, Stata, Excel,...)")

    }

  } else{

    print("La media poblacional no suele conocerse, este supuesto es te\u00f3rico")

    media <- readline(prompt = "Introducir el valor de la media poblacional: ")
    media <- as.numeric(media)
    sumatorio <- readline(prompt = "Introducir el valor de la suma cuadratica de las desviaciones de los valores muestrales respecto a la media poblacional: ")
    sumatorio <- as.numeric(sumatorio)

  }

}

# calculo de los intervalos de confianza

if(media_poblacion == "desconocida"){
  # caso 1. Media poblacional desconocida, n peque\u00f1a
  print("Intervalo de confianza para la varianza poblacional, supuesta desconocida la media poblacional. n peque\u00f1a")

  limite_inferior <- (n * var_mu) / qchisq(alfa_2,lower.tail = F, df= gl)
  limite_superior <- (n * var_mu) / qchisq(1-alfa_2, lower.tail = F, df= gl)

} else{

  # caso 2. Media poblacional conocida, n peque\u00f1a
  print("Intervalo de confianza para la varianza poblacional, supuesta conocida la media poblacional. n peque\u00f1a")

  limite_inferior <- sumatorio / qchisq(alfa_2,lower.tail = F, df= gl+1)
  limite_superior <- sumatorio / qchisq(1-alfa_2, lower.tail = F, df= gl+1)

}


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}
