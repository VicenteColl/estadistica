#' @title Contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' @description Realiza el contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' @usage contraste.diferencia.proporciones(x,
#'                 variable = NULL,
#'                 introducir = FALSE,
#'                 hipotesis_nula = 0,
#'                 tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                 p_muestral = c(1,1),
#'                 alfa = 0.05,
#'                 grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico. Por defecto el valor está fijado en cero.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, tipo_contraste = "bilateral".
#' Si tipo_contraste = "bilateral", se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#' Si tipo_contraste = "cola derecha", se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#' Si tipo_contraste = "cola izquierda", se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param p_muestral Es un vector de longitud 2 cuyo primer elemento hará referencia a qué valor se toma para la proporción de la muestra 1 y el segundo al de la muestra 2.
#' El valor 1 indica que se toma la proporción de la muestra, el valor 2 indica que se toma el caso más desfavorable (p=q=0.5)
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, alfa = 0.05 (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.
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
#' @import dplyr ggplot2
#'
#' @export
contraste.diferencia.proporciones <- function(x,
                                     variable = NULL,
                                     introducir = FALSE,
                                     hipotesis_nula = 0,
                                     tipo_contraste =  c("bilateral","cola derecha","cola izquierda"),
                                     p_muestral = c(1,1),
                                     alfa = 0.05,
                                     grafico = FALSE){


  tipo_contraste <- tolower(tipo_contraste)
  tipo_contraste <- match.arg(tipo_contraste)

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
    stop("El nivel de significacion debe fijarse entre 0 y 1")
  }

  if(hipotesis_nula >= 0 & hipotesis_nula <=1){
    H0 <- hipotesis_nula
  }else{
    stop("La hipotesis nula es una proporcion y por tanto tiene que fijarse entre 0 y 1")
  }



  if(isFALSE(introducir)) {

    print("En tus datos tiene que haber una variable de agrupación y una variable con éxitos(=1) y fracasos(=0)")

    if(is.null(variable)){

      if(length(x) == 2){

        x <- data.frame(x)

        agrupacion <- readline(prompt="Indica la posición (número de columna) de la variable de agrupación: ")
        agrupacion <- as.numeric(agrupacion)

        x <- x %>%
          select(all_of(agrupacion),everything())
        x <- na.omit(x)


      } else{

        stop("El conjunto de datos seleccionado no tiene 2 variables.")

      }

    } else if(length(x) > 2){

      agrupacion <- readline(prompt="Indica la posición (número de columna) de la variable de agrupación: ")
      agrupacion <- as.numeric(agrupacion)

      exito <- readline(prompt="Indica la posición (número de columna) de la variable con éxitos (=1) y fracasos (=0): ")
      exito <- as.numeric(exito)

      variable <- c(agrupacion,exito)

      x <- x[,variable] %>% as.data.frame()
      varnames <- names(x)
      x <- na.omit(x)


    } else{

      stop("Seleccion erronea de variables para calcular el IC")

    }


    if(!all(x[,2] == 0 | x[,2]==1)){

      print("Aplica a tus datos la condicion que debe cumplir la poblacion para transfomar los datos en ceros (ausencia/no exito) y unos (presencia/exito)")
      stop("Los valores en la muestra deben ser 0 y 1.")

    }

    if(!length(unique(x[,1]))==2){

      stop("La variable de agrupación no tiene dos categorías")

    }

    df <- table(x)


  # tamaño de la muestra
  n <- apply(df,1,sum)
  n1 <- as.numeric(n[1])
  n2 <- as.numeric(n[2])

  # media muestral
  p_mu1 <- as.numeric(df[1,2]/n1)
  p_mu2 <- as.numeric(df[2,2]/n2)


} else{   # aquí empieza introducir datos

  print("Primero vas a introducir los datos de la muestra 1 y a continuación introducirás los datos de la muestra 2")
  print("Si los datos provienen de encuestas realizadas antes y después de una determinada acción, introduce primero los datos de la encuesta realizada después de dicha acción")

  n1 <- readline(prompt = "Introducir el tamaño de la muestra 1: ")
  n1 <- as.numeric(n1)

  if(p_muestral[1] == 2){

    p_mu1 <- 0.5

  } else{
    p_mu1 <- readline(prompt = "Introducir el valor de la proporcion muestral 1: ")
    p_mu1 <- as.numeric(p_mu1)
  }

  n2 <- readline(prompt = "Introducir el tamaño de la muestra 2: ")
  n2 <- as.numeric(n2)

  if(p_muestral[2] == 2){

    p_mu2 <- 0.5

  } else{
    p_mu2 <- readline(prompt = "Introducir el valor de la proporcion muestral 2: ")
    p_mu2 <- as.numeric(p_mu2)
  }

}


  # calculo de los contrastes
  # estadistico de prueba
  dif_p <- p_mu1 - p_mu2


  #error_tipico <- sqrt((p_mu1 * (1-p_mu1))/n1 + (p_mu2 * (1-p_mu2))/n2)
  est_p <- (n1*p_mu1 + n1*p_mu2)/(n1+n2)
  error_tipico <- sqrt(((n1+n2)/(n1*n2)) * est_p * (1-est_p))

  estadistico.Z <- (dif_p - H0)/error_tipico
  estadistico.Z <- round(estadistico.Z,5)

  if(tipo_contraste == "bilateral"){

    estadistico.Z2 <- abs(estadistico.Z)
    pvalor <- 2*pnorm(estadistico.Z2,lower.tail=FALSE)

    if(estadistico.Z >= -valor_critico & estadistico.Z <=  valor_critico){

      print(paste("No se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) se encuentra dentro de la región de aceptación")

    } else{

      print(paste("Se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) no se encuentra dentro de la región de aceptación")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = -estadistico.Z2, linetype = "dashed") +
        geom_vline(xintercept = estadistico.Z2, linetype = "dashed") +
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z2,-estadistico.Z2,-valor_critico,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  } else if(tipo_contraste == "cola derecha"){

    pvalor <- pnorm(estadistico.Z,lower.tail=FALSE)


    if(estadistico.Z > valor_critico){

      print(paste("Se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) no se encuentra dentro de la región de aceptación")

    } else{

      print(paste("No se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) se encuentra dentro de la región de aceptación")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-4,valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  } else{

    pvalor <- pnorm(estadistico.Z,lower.tail=TRUE)


    if(estadistico.Z < valor_critico){

      print(paste("Se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) no se encuentra dentro de la región de aceptación")

    } else{

      print(paste("No se rechaza la hipotesis nula. La región de aceptación viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estadístico de prueba (o valor experimental) se encuentra dentro de la región de aceptación")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, 4)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,-valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  }


  CH <- cbind(H0,estadistico.Z,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hipótesis nula", "estadístico de prueba", "p-valor")
  row.names(CH) <- NULL

  if(isTRUE(grafico)){

    return(list(CH,plot))

  } else{

    return(CH)

  }

}


