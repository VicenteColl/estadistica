#' @title Intervalo confianza para la razón (cociente) de varianzas.
#'
#' @description Calcula el intervalo de confianza para la razón (o cociente) de varianzas.
#'
#' \figure{qr_ic.razon.varianzas.png}{options: "center" width="25\%" heigth="25\%"}
#'
#' @usage ic.razon.varianzas(x, variable = NULL, introducir = FALSE, confianza = 0.95)
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
ic.razon.varianzas <- function(x,
                               variable = NULL,
                               introducir = FALSE,
                               confianza = 0.95){

print("Se calcula el intervalo de confianza para el cociente de varianzas supuestas desconocidas las medias poblacionales")

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
        warning("Para calcular el intervalo de confianza de la raz\u00f3n de varianzas hay que seleccionar dos variables")
        stop("El conjunto de datos seleccionado parece ser no v\u00e1lido")
      }
  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))){
    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")
  }

  # tama\u00f1o de la muestra
  n1 <- length(x[1][!is.na(x[1])])
  n2 <- length(x[2][!is.na(x[2])])


  var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

  if(var_muestra == 1){
    var_mu1 <- varianza(x[1])
    var_mu2 <- varianza(x[2])

    numerador <- n2 * (n1-1) * var_mu2
    denominador <- (n2-1) * n1 * var_mu1

  } else{

    var_mu1 <- varianza(x[1], tipo = "cuasi")
    var_mu2 <- varianza(x[2], tipo = "cuasi")

  }

} else{   # aqu\u00ed empieza introducir datos

  print("A continuaci\u00f3n, vas a introducir los datos de las muestras.")

  n1 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 1: ")
  n1 <- as.numeric(n1)

  n2 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 2: ")
  n2 <- as.numeric(n2)

  var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu1 <- readline("Introduce el valor de la varianza muestral 1: ")
      var_mu1 <- as.numeric(var_mu1)

      var_mu2 <- readline("Introduce el valor de la varianza muestral 2: ")
      var_mu2 <- as.numeric(var_mu2)

      numerador <- n1 * (n2-1) * var_mu1
      denominador <- (n1-1) * n2 * var_mu2

    } else{

      var_mu1 <- readline("Introduce el valor de la cuasivarianza muestral 1: ")
      var_mu1 <- as.numeric(var_mu1)

      var_mu2 <- readline("Introduce el valor de la cuasivarianza muestral 2: ")
      var_mu2 <- as.numeric(var_mu2)

    }

}

# calculo del intervalo de confianza

# IC del cociente de varianzas con medias desconocidas

if(var_muestra == 1){

  # caso 1.1
  limite_inferior <- (numerador / denominador) * qf(1-alfa_2, df1= n2-1, df2 = n1-1,lower.tail = F)
  limite_superior <- (numerador / denominador) * qf(alfa_2, df1= n2-1, df2 = n1-1, lower.tail = F)

} else {

  # caso 1.2
  print("Este es el intervalo de confianza que generalmente calculan los softwares (SPSS, Excel, etc.")

  limite_inferior <- (var_mu1/var_mu2) * qf(1-alfa_2, df1= n2-1, df2 = n1-1, lower.tail = F)
  limite_superior <- (var_mu1/var_mu2) * qf(alfa_2, df1= n2-1, df2 = n1-1,lower.tail = F)

}


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}
