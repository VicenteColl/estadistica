#' @title Contraste de hipótesis de correlación
#'
#' @description Realiza el contraste de hipótesis sobre el coeficiente de correlación.
#'
#' @usage contraste.correlacion(x,
#'                 variable = NULL,
#'                 introducir = FALSE,
#'                 hipotesis_nula = 0,
#'                 tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                 alfa = 0.05)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico. Por defecto el valor está fijado a cero (incorrelación).
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, tipo_contraste = "bilateral".
#'                       Si tipo_contraste = "bilateral", se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#'                       Si tipo_contraste = "cola derecha", se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#'                       Si tipo_contraste = "cola izquierda", se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, alfa = 0.05 (5 por ciento)
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
#' El estadístico del contraste es:
#'
#' \figure{c_correlacion.png}{options: width="30\%" heigth="30\%"}
#'
#' que se distribuye como una t con n-2 grados de libertad.
#'
#' @seealso \code{\link{ic.correlacion}}
#'
#' @references
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import dplyr ggplot2
#'
#' @export
contraste.correlacion <- function(x,
                                  variable = NULL,
                                  introducir = FALSE,
                                  hipotesis_nula = 0,
                                  tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                                  alfa = 0.05){


  print("Se asume que la variable bivariante (X,Y) se distribuye conjuntamente normal")
  print("El contraste de independencia es equivalente a contrastar que el coeficiente de correlaci\u00f3n es cero frente a la alternativa de que es distinto de cero")

  warning("Actualmente solo se encuentra implementado el contraste de incorrelaci\u00f3n")

  tipo_contraste <- tolower(tipo_contraste)
  tipo_contraste <- match.arg(tipo_contraste)


if(is.null(hipotesis_nula) | !is.numeric(hipotesis_nula)){

  stop("Tienes que introducir un valor para la hip\u00f3tesis nula")

} else{

  H0 <- hipotesis_nula

}


if(alfa >= 0 & alfa <=1){

  if(tipo_contraste == "bilateral"){
    valor_critico <- qnorm(alfa/2,lower.tail = F)
  }
  if(tipo_contraste == "cola izquierda"){
    valor_critico <- qnorm(alfa,lower.tail = T)
  }
  if(tipo_contraste == "cola derecha"){
    valor_critico <- qnorm(alfa,lower.tail = F)
  }

  valor_critico <- round(valor_critico,4)

} else{
  stop("El nivel de significaci\u00f3n debe fijarse entre 0 y 1")
}


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){
      x <- x
    } else{
      warning("Para calcular el contraste hay que seleccionar 2 variables")
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
        warning("Para calcular el contraste de correlaci\u00f3 hay que seleccionar dos variables")
        stop("El conjunto de datos seleccionado parece ser no v\u00e1lido")
      }
  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))){
    stop("No puede calcularse el contraste porque la variable seleccionada no es cuantitativa")
  }

  # tama\u00f1o de la muestra
  x <- na.omit(x)
  n <- nrow(x)
  correlacion <- as.numeric(correlacion(x))

} else{   # aqu\u00ed empieza introducir datos

  print("A continuaci\u00f3n, vas a introducir los datos muestrales.")

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra : ")
  n <- as.numeric(n)

  correlacion <- readline(prompt = "Introducir el valor del coeficiente de correlaci\u00f3n muestral: ")
  correlacion <- as.numeric(correlacion)

  if((correlacion < -1 | correlacion > 1)){

    stop("El coeficiente de correlaci\u00f3n debe estar comprendido entre -1 y 1.")

  }

}


if(hipotesis_nula == 0){

  estadistico.prueba <- sqrt((correlacion^2 * (n - 2)) / (1 - correlacion^2))

  if(tipo_contraste == "bilateral"){

    valor_critico <- round(qt(alfa/2,n-2,lower.tail=FALSE),4)

    pvalor <- 2 * pt(estadistico.prueba, n-2,lower.tail=FALSE)


    if((estadistico.prueba > - valor_critico & estadistico.prueba < valor_critico)){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")


    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

  } else if(tipo_contraste == "cola derecha"){

    valor_critico <- round(qt(alfa,n-2,lower.tail=FALSE),4)

    pvalor <- pt(estadistico.prueba, n-2,lower.tail=FALSE)


    if((estadistico.prueba <= valor_critico)){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -Inf," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")


    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -Inf," , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }


  } else{

    valor_critico <- round(qt(alfa,n-2,lower.tail=FALSE),4)

    pvalor <- pt(estadistico.prueba, n-2,lower.tail=FALSE)


    if((estadistico.prueba >= -valor_critico)){

      print(paste("No se rechaza la hip\u00f3\tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",Inf,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")


    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ", Inf,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }



  }


} else{


  estadistico.prueba <- 0.5*log((1+correlacion)/(1-correlacion))

  limite_inferior <- round(0.5 * log((1+H0)/(1-H0)) - valor_critico * sqrt(1/(n - 3)),5)
  limite_superior <- round(0.5 * log((1+H0)/(1-H0)) + valor_critico * sqrt(1/(n - 3)),5)


  if(tipo_contraste == "bilateral"){

    pvalor <- 2 * pnorm(estadistico.prueba,lower.tail=FALSE)


    if(estadistico.prueba > limite_inferior & estadistico.prueba < limite_superior){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", limite_inferior," , ",limite_superior,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")


    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", limite_inferior," , ",limite_superior,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

  }

}


  CH <- cbind(H0,estadistico.prueba,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  return(CH)

}
