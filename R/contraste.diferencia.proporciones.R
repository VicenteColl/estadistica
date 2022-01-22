#' @title Contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' @description Realiza el contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcdifproporciones.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcdifproporciones.png}{options: width=3cm}}
#'
#' @usage contraste.diferencia.proporciones(x,
#'                  variable = NULL,
#'                  introducir = FALSE,
#'                  hipotesis_nula = 0,
#'                  tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                  alfa = 0.05,
#'                  grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere solo a dos variables, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico. Por defecto el valor está fijado en cero.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, \code{tipo_contraste = "bilateral"}.
#'        Si \code{tipo_contraste = "bilateral"}, se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#'        Si \code{tipo_contraste = "cola derecha"}, se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#'        Si \code{tipo_contraste = "cola izquierda"}, se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return La función devuelve un objeto de la clase \code{list}. La lista contendrá información sobre: la hipótesis nula contrastada, el estadístico de prueba, el p-valor  el intervalo de confianza para la diferencia de proporciones muestrales supuesta cierta la hipótesis nula. Si \code{grafico=TRUE} se incluirá una representación gráfica de la región de aceptación-rechazo.
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
#' El estadístico Z del contraste, que se distribuye N(0,1), es:
#'
#' (1) Si se consideran las proporciones muestrales:
#'
#' \if{html}{\figure{cdifpromuestra.png}{options: width="60\%" alt="Figure: cdifpromuestra.png"}}
#' \if{latex}{\figure{cdifpromuestra.png}{options: width=8cm}}
#'
#' (2) si se estima p como media ponderada de las proporciones muestrales, la ponderación es:
#'
#' \if{html}{\figure{cproponderacion.png}{options: width="50\%" alt="Figure: cproponderacion.png"}}
#' \if{latex}{\figure{cproponderacion.png}{options: width=6cm}}
#'
#' y el estadístico resulta:
#'
#' \if{html}{\figure{cdifpropond.png}{options: width="60\%" alt="Figure: cdifpropond.png"}}
#' \if{latex}{\figure{cdifpropond.png}{options: width=7cm}}
#'
#' @seealso \code{\link{ic.diferencia.proporciones}}
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
contraste.diferencia.proporciones <- function(x,
                                     variable = NULL,
                                     introducir = FALSE,
                                     hipotesis_nula = 0,
                                     tipo_contraste =  c("bilateral","cola derecha","cola izquierda"),
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
    stop("La hip\u00f3tesis nula es una proporcion y por tanto tiene que fijarse entre 0 y 1")
  }



  if(isFALSE(introducir)) {

    x <- data.frame(x)
    varnames <- names(x)

    if(is.null(variable)){

      if(length(x) == 2){

        x <- x

      } else{

        warning("Para realizar el contraste hay que seleccionar dos variables")
        stop("El conjunto de datos seleccionado tiene mas de 2 variables.")

      }

    } else if(length(variable) == 2){

      if(is.numeric(variable)){

        if(all(variable <= length(x))){

          variable <- variable

        } else{

          stop("Selecci\u00f3n err\u00f3nea de variables")

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

      warning("Para realizar el contraste hay que seleccionar dos variables")
      stop("El conjunto de datos seleccionado no es adecuado.")

    }

    clase <- sapply(x, class)

    if (!all(clase %in% c("numeric","integer"))) {

      stop("No puede calcularse el contraste porque las variables seleccionadas no son cuantitativas")

    }

    x1 <- na.omit(x[1])
    x2 <- na.omit(x[2])


    if(!all((x1 == 0) | x1 ==1)){

      print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9xito) y unos (presencia/\u00e9xito)")
      stop("Los valores en la muestra deben ser 0 y 1.")

    }

    if(!all((x2 == 0) | x2 ==1)){

      print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9xito) y unos (presencia/\u00e9xito)")
      stop("Los valores en la muestra deben ser 0 y 1.")

    }


    # tama\u00f1o de la muestra
    n1 <- nrow(x1)
    n2 <- nrow(x2)

    # media muestral
    p_mu1 <- round(sum(x1)/n1,6)
    p_mu2 <- round(sum(x2)/n2,6)

} else{   # aqu\u00ed empieza introducir datos

  print("Primero vas a introducir los datos de la muestra 1 y a continuaci\u00f3n introducir\u00e1s los datos de la muestra 2")
  print("Si los datos provienen de encuestas realizadas antes y despu\u00e9s de una determinada acci\u00f3n, introduce primero los datos de la encuesta realizada despu\u00e9s de dicha acci\u00f3n")

  n1 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 1: ")
  n1 <- as.numeric(n1)

  p_mu1 <- readline(prompt = "Introducir el valor de la proporcion muestral 1: ")
  p_mu1 <- as.numeric(p_mu1)


  n2 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 2: ")
  n2 <- as.numeric(n2)


  p_mu2 <- readline(prompt = "Introducir el valor de la proporcion muestral 2: ")
  p_mu2 <- as.numeric(p_mu2)


}

  # calculo de los contrastes

  est_proporcion <- as.numeric(readline('Selecciona el valor que quieres utilizar para el error t\u00edpico bajo la H0: \n 1. "Estimar p como media ponderada de las proporciones muestrales" \n 2. "Utilizar las proporciones muestrales" \n'))

  # estadistico de prueba
  dif_p <- p_mu1 - p_mu2

  if(est_proporcion == 1){

    est_p <- ((n1*p_mu1) + (n2*p_mu2))/(n1+n2)
    error_tipico <- sqrt(((n1+n2)/(n1*n2)) * est_p * (1-est_p))

  } else{

    error_tipico <- sqrt((p_mu1 * (1-p_mu1))/n1 + (p_mu2 * (1-p_mu2))/n2)

  }

  estadistico.Z <- (dif_p - H0)/error_tipico
  estadistico.Z <- round(estadistico.Z,5)

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
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = -estadistico.Z2, linetype = "dashed") +
        geom_vline(xintercept = estadistico.Z2, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo para\nla diferencia de proporciones") +
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
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-4,valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo para\nla diferencia de proporciones") +
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
        geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
        geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, 4)) +
        geom_vline(xintercept = estadistico.Z, linetype = "dashed") +
        labs(x = "", y = "",title="Regi\u00f3n de aceptaci\u00f3n-rechazo para\nla diferencia de proporciones") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,-valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  }


  CH <- cbind(H0,estadistico.Z,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  Idifpro <- cbind(`limite_inferior`=media_inf,`limite_superior`=media_sup)


  if(grafico){

    return(list(`Estadistico`=CH,`Intervalo de la proporcion muestral (supuesta H0 cierta)`= Idifpro,`Graficos`= plot))

  } else{

    return(list(`Estadistico`=CH,`Intervalo de la proporcion muestral (supuesta H0 cierta)`= Idifpro))

  }

}


