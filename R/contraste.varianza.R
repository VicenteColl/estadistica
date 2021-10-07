#' @title Contraste de hipótesis sobre la varianza.
#'
#' @description Realiza el contraste de hipótesis sobre la varianza poblacional.
#'
#' @usage contraste.varianza(x,
#'                        variable = NULL,
#'                        introducir = FALSE,
#'                        media_poblacion = c("desconocida","conocida"),
#'                        hipotesis_nula = NULL,
#'                        tipo_contraste =  c("bilateral","cola derecha","cola izquierda"),
#'                        alfa = 0.05,
#'                        grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param media_poblacion Es un carácter. Indica si la media de la población es desconocida (por defecto, media_poblacion = "desconocida") o conocida (en este caso, cambiar media_poblacion = "conocida").
#' @param hipotesis_nula Es un valor numérico.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, tipo_contraste = "bilateral".
#' Si tipo_contraste = "bilateral", se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#' Si tipo_contraste = "cola derecha", se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#' Si tipo_contraste = "cola izquierda", se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, alfa = 0.05 (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.
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
#' (1) Si la media poblacional es conocida, el estadístico chi-dos es:
#'
#' (1.1) utilizando la varianza muestral:
#'
#' \figure{c_var_muestra.png}{options: width="30\%" heigth="30\%"}
#'
#' (1.2) utilizando la cuasi-varianza muestral:
#'
#' \figure{c_var_cuasi.png}{options: width="30\%" heigth="30\%"}
#'
#' (2) Si la media poblacional es conocida.
#'
#' (2.1) utilizando la varianza muestral:
#'
#' \figure{c_var_media_con_muestra.png}{options: width="60\%" heigth="60\%"}
#'
#'  Nota: En todos los casos el estadístico chi-dos se distrubuye con n-1 grados de libertad.
#'
#' @seealso \code{\link{ic.varianza}}
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
contraste.varianza <- function(x,
                               variable = NULL,
                               introducir = FALSE,
                               media_poblacion = c("desconocida","conocida"),
                               hipotesis_nula = NULL,
                               tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                               alfa = 0.05,
                               grafico = FALSE){


  media_poblacion <- tolower(media_poblacion)
  media_poblacion <- match.arg(media_poblacion)


tipo_contraste <- tolower(tipo_contraste)
tipo_contraste <- match.arg(tipo_contraste)

if(is.null(hipotesis_nula) | !is.numeric(hipotesis_nula)){

  stop("Tienes que introducir un valor para la hip\u00f3tesis nula")

} else{
  H0 <- hipotesis_nula

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

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra
  n <- nrow(x)

  if(media_poblacion == "desconocida"){
    gl <- n-1 # grados de libertad
  } else{
    gl <- n # grados de libertad
  }

  if(media_poblacion == "desconocida"){

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu <- as.numeric(varianza(x))

    } else{

      var_mu <- as.numeric(varianza(x, tipo = "cuasi"))
      n <- n-1

    }
  } else{

    print("La media poblacional no suele conocerse, este supuesto es teorico")

    media <- readline(prompt = "Introducir el valor de la media poblacional: ")
    media <- as.numeric(media)

    sumatorio <- sum((x - media)^2)

  }


} else{   # aquí empieza introducir datos


  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n <- as.numeric(n)

  if(media_poblacion == "desconocida"){
    gl <- n-1 # grados de libertad
  } else{
    gl <- n # grados de libertad
  }

  if(media_poblacion == "desconocida"){

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu <- readline("Introduce el valor de la varianza muestral: ")
      var_mu <- as.numeric(var_mu)

    } else{

      var_mu <- readline("Introduce el valor de la cuasivarianza muestral: ")
      var_mu <- as.numeric(var_mu)
      n <- n-1

    }

  } else{

    print("La media poblacional no suele conocerse, este supuesto es teorico")

    #media <- readline(prompt = "Introducir el valor de la media poblacional: ")
    #media <- as.numeric(media)
    sumatorio <- readline(prompt = "Introducir el valor de la suma cuadratica de las desviaciones de los valores muestrales respecto a la media poblacional: ")
    sumatorio <- as.numeric(sumatorio)

  }

}


if(alfa >= 0 & alfa <=1){

    if(tipo_contraste == "bilateral"){
      valor_critico_L <- round(qchisq(alfa/2,gl,lower.tail = T),4)
      valor_critico_R <- round(qchisq(alfa/2,gl,lower.tail = F),4)

    }
    if(tipo_contraste == "cola izquierda"){
      valor_critico <- round(qchisq(alfa,gl,lower.tail = T),4)
    }
    if(tipo_contraste == "cola derecha"){
      valor_critico <- round(qchisq(alfa,gl,lower.tail = F),4)
    }

} else{
  stop("El nivel de significacion debe fijarse entre 0 y 1")
}


# caluclo de los contrastes

if(media_poblacion == "desconocida"){

  estadistico.prueba <- n * var_mu / H0

} else{

  estadistico.prueba <- sumatorio / H0

}

percentil99 <- qchisq(.9999, gl)

df <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
df$y <-dchisq(df$x, gl)

  if(tipo_contraste == "bilateral"){

    limite_inf <- round(H0 /n * valor_critico_L,4)
    limite_sup <- round(H0 /n * valor_critico_R,4)
    pvalor <- 2 *min(pchisq(estadistico.prueba,gl,lower.tail=F),pchisq(estadistico.prueba,gl,lower.tail=T))

    if(estadistico.prueba >= valor_critico_L & estadistico.prueba <= valor_critico_R){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico_L," , ",valor_critico_R,"]",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico_L," , ",valor_critico_R,"]",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(df) +
        geom_path(aes(x,y))+
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "darkgreen", xlim = c(valor_critico_L, valor_critico_R)) +
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "red", xlim = c(0, valor_critico_L)) +
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "red", xlim = c(valor_critico_R, percentil99)) +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
        labs(title = paste("Distribuci\u00f3n chi con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(0L,estadistico.prueba,valor_critico_L,valor_critico_R)) +
        theme(axis.text.x = element_text(angle = 45))
    }


  } else if(tipo_contraste == "cola derecha"){

    limite_inf <- 0
    limite_sup <- round(H0 /n * valor_critico,4)
    pvalor <- pchisq(estadistico.prueba,gl,lower.tail= F)


    if(estadistico.prueba <=  valor_critico){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", 0 ," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", 0," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(df) +
        geom_path(aes(x,y))+
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "darkgreen", xlim = c(0, valor_critico)) +
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "red", xlim = c(valor_critico,percentil99)) +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
        labs(title = paste("Distribuci\u00f3n chi con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(0L,estadistico.prueba,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  } else{

    limite_inf <- round(H0 /n * valor_critico,4)
    limite_sup <- Inf
    pvalor <- pchisq(estadistico.prueba,gl,lower.tail=T)


    if(estadistico.prueba >= valor_critico){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico," , ",Inf,"[",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", valor_critico," , ",Inf,"[",sep=""))
      print("El valor del estad\u00ed de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(df) +
        geom_path(aes(x,y))+
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "darkgreen", xlim = c(valor_critico,percentil99)) +
        geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "red", xlim = c(0,valor_critico)) +
        geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
        geom_vline(xintercept = 0, color = "black") +
        labs(title = paste("Distribuci\u00f3n chi con ", gl, " grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(0L,estadistico.prueba,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }



  }


  CH <- cbind(H0,estadistico.prueba,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p valor")
  row.names(CH) <- NULL

  if(isTRUE(grafico)){

    return(list(CH,plot))

  } else{

    return(CH)

  }


}
