#' @title Contraste de hipótesis sobre la proporción.
#'
#' @description Realiza el contraste de hipótesis sobre la proporción poblacional.
#'
#' @usage contraste.proporcion(x,
#'                        variable = NULL,
#'                        introducir = FALSE,
#'                        hipotesis_nula = NULL,
#'                        tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                        p_muestral = TRUE,
#'                        alfa = 0.05,
#'                        grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, tipo_contraste = "bilateral".
#' Si tipo_contraste = "bilateral", se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#' Si tipo_contraste = "cola derecha", se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#' Si tipo_contraste = "cola izquierda", se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param p_muestral Es un valor lógico. Indica si se hace uso de la proporción muestral para estimar el intervalo de confianza (por defecto, p_muestral = TRUE). si p_muestral = FALSE, se considera p=q=0.5 (situación más desfavorable).
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
contraste.proporcion <- function(x,
                                 variable = NULL,
                                 introducir = FALSE,
                                 hipotesis_nula = NULL,
                                 tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                                 p_muestral = TRUE,
                                 alfa = 0.05,
                                 grafico = FALSE){



  tipo_contraste <- tolower(tipo_contraste)
  tipo_contraste <- match.arg(tipo_contraste)

  if(is.null(hipotesis_nula)){

    stop("Tienes que introducir un valor para la hipotesis nula")

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
    stop("El nivel de significacion debe fijarse entre 0 y 1")
  }

  if(hipotesis_nula >= 0 & hipotesis_nula <=1){
    H0 <- hipotesis_nula
  }else{
    stop("La hipotesis nula es una proporcion y por tanto tiene que fijarse entre 0 y 1")
  }


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 1){

      x <- x

    } else{

      warning("Para calcular el contraste hay que seleccionar una variable")
      stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

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

    warning("Para calcular el contraste hay que seleccionar una variable")
    stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

  }

  if(!all(x == 0 | x==1)){

    print("Aplica a tus datos la condicion que debe cumplir la poblacion para transfomar los datos en ceros (ausencia/no exito) y unos (presencia/exito)")
    stop("Los valores en la muestra deben ser 0 y 1.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el contraste porque la variable seleccionada no es cuantitativa")

  }

  # tamaño de la muestra
  n <- nrow(x)

  # media muestral

  if(isTRUE(p_muestral)){
    p_mu <- sum(x,na.rm=TRUE)/n
  } else{
    p_mu = 0.5
  }


} else{   # aquí empieza introducir datos

  n <- readline(prompt = "Introducir el tamaño de la muestra: ")
  n <- as.numeric(n)

  if(isTRUE(p_muestral)){

    p_mu <- readline(prompt = "Introducir el valor de la proporcion muestral: ")
    p_mu <- as.numeric(p_mu)

  } else{
    p_mu <- 0.5
  }

}

  # calculo de los contrastes
  # estadistico de prueba

  estadistico.Z <- (p_mu - H0)/sqrt(H0*(1-H0)/n)
  estadistico.Z <- round(estadistico.Z,5)
  error_tipico0 <- sqrt(H0*(1-H0)/n)

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

      plot <- ggplot(NULL, aes(c(-3,3))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-3, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 3)) +
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

      plot <- ggplot(NULL, aes(c(-3,3))) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-3L,valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 3L)) +
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

      plot <- ggplot(NULL, aes(c(-3,3))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-3L, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(valor_critico, 3L)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,valor_critico)) +
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




