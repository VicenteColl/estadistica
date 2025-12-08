#' @title Contraste de hipótesis sobre la proporción.
#'
#' @description Realiza el contraste de hipótesis sobre la proporción poblacional.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcproporcion.png}{options:style="width: 25\%;" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcproporcion.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, \code{tipo_contraste = "bilateral"}.
#'        Si \code{tipo_contraste = "bilateral"}, se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#'        Si \code{tipo_contraste = "cola derecha"}, se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#'        Si \code{tipo_contraste = "cola izquierda"}, se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return La función devuelve un objeto de la clase \code{list}. La lista contendrá información sobre: la hipótesis nula contrastada, el estadístico de prueba, el p-valor y  el intervalo de confianza para la proporción muestral supuesta cierta la hipótesis nula. Si \code{grafico=TRUE} se incluirá una representación gráfica de la región de aceptación-rechazo con los valores críticos.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' En este caso el estadístico Z del contraste es:
#'
#' \if{html}{\figure{cproporcion.png}{options: style="width: 40\%;" alt="Figure: cproporcion.png"}}
#' \if{latex}{\figure{cproporcion.png}{options: width=4cm}}
#'
#' @seealso \code{\link{ic.proporcion}}
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
#' @importFrom stats pnorm na.omit dnorm
#' @import dplyr ggplot2
#'
#' @export
contraste.proporcion <- function(x,
                                 variable = NULL,
                                 introducir = FALSE,
                                 hipotesis_nula = NULL,
                                 tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                                 alfa = 0.05,
                                 grafico = FALSE){

  tipo_contraste <- tolower(tipo_contraste)
  tipo_contraste <- match.arg(tipo_contraste)

  if(is.null(hipotesis_nula)){

    stop("Tienes que introducir un valor para la hipo\u00f3tesis nula")

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

  if(hipotesis_nula >= 0 & hipotesis_nula <=1){
    H0 <- hipotesis_nula
  }else{
    stop("La hip\u00f3tesis nula es una proporci\u00f3n y por tanto tiene que fijarse entre 0 y 1")
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

    warning("Para calcular el contraste hay que seleccionar una variable")
    stop("El conjunto de datos seleccionado tiene mas de 1 variable.")

  }

  if(!all(x == 0 | x==1)){

    print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9xito) y unos (presencia/\u00e9xito)")
    stop("Los valores en la muestra deben ser 0 y 1.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el contraste porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra
  n <- nrow(x)

  # media muestral

    p_mu <- sum(x,na.rm=TRUE)/n

} else{   # aqu\u00ed empieza introducir datos

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n <- as.numeric(n)

  p_mu <- readline(prompt = "Introducir el valor de la proporcion muestral: ")
  p_mu <- as.numeric(p_mu)

}

  # calculo de los contrastes
  # estadistico de prueba

  estadistico.Z <- (p_mu - H0)/sqrt(H0*(1-H0)/n)
  estadistico.Z <- round(estadistico.Z,5)
  error_tipico <- sqrt(H0*(1-H0)/n)

  if(tipo_contraste == "bilateral"){

    estadistico.Z2 <- abs(estadistico.Z)
    pvalor <- 2*pnorm(estadistico.Z2,lower.tail=FALSE)
    media_inf <- H0 - valor_critico * error_tipico
    media_sup <- H0 + valor_critico * error_tipico

    if(estadistico.Z >= -valor_critico & estadistico.Z <=  valor_critico){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-3, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 3)) +
        geom_vline(xintercept = -estadistico.Z2, linetype = "dashed") +
        geom_vline(xintercept = estadistico.Z2, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z2,-estadistico.Z2,-valor_critico,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

    }

  } else if(tipo_contraste == "cola derecha"){

    media_inf <- -Inf
    media_sup <- H0 + valor_critico * error_tipico
    pvalor <- pnorm(estadistico.Z,lower.tail=FALSE)

    if(estadistico.Z > valor_critico){

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-3L,valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 3L)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

    }

  } else{

    media_inf <- H0 + valor_critico * error_tipico # valor critico negativo
    media_sup <- Inf
    pvalor <- pnorm(estadistico.Z,lower.tail=TRUE)

    if(estadistico.Z < valor_critico){

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

    if(isTRUE(grafico)){

      plot <- ggplot(NULL, aes(c(-4,4))) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-3L, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(valor_critico, 3L)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))

    }

  }

  CH <- cbind(H0,estadistico.Z,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  Iproporcion <- cbind(`limite_inferior`=media_inf,`limite_superior`=media_sup)


  if(grafico){

    return(list(`Estadistico`=CH,`Intervalo de la proporcion muestral (supuesta H0 cierta)`= Iproporcion,`Graficos`= plot))

  } else{

    return(list(`Estadistico`=CH,`Intervalo de la proporcion muestral (supuesta H0 cierta)`= Iproporcion))

  }

}




