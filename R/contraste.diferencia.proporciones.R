#' @title Contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' @description Realiza el contraste de hipótesis sobre la diferencia de dos proporciones.
#'
#' @usage contraste.diferencia.proporciones(x,
#' variable = NULL,
#' introducir = FALSE,
#' hipotesis_nula = 0,
#' tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#' alfa = 0.05,
#' grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param hipotesis_nula Es un valor numérico. Por defecto el valor está fijado en cero.
#' @param tipo_contraste Es un carácter. Indica el tipo de contraste a realizar. Por defecto, tipo_contraste = "bilateral".
#' Si tipo_contraste = "bilateral", se contraste la hipótesis nula igual un valor frente a la alternativa distinto de dicho valor.
#' Si tipo_contraste = "cola derecha", se contrasta la hipótesis nula menor o igual a un valor frente a la alternativa mayor a dicho valor.
#' Si tipo_contraste = "cola izquierda", se contrasta la hipótesis nula mayor o igual a un valor frente a la alternativa menos a dicho valor.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, alfa = 0.05 (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.
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
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' El estadístico Z del contraste, que se distribuye N(0,1), es:
#'
#' (1) Si se consideran las proporciones muestrales:
#'
#' \figure{c_dif_pro_muestra.png}{options: width="60\%" heigth="60\%"}
#'
#' (2) si se estima p como media ponderada de las proporciones muestrales, la ponderación es:
#'
#' \figure{c_pro_ponderacion.png}{options: width="50\%" heigth="50\%"}
#'
#' y el estadístico resulta:
#'
#' \figure{c_dif_pro_pond.png}{options: width="60\%" heigth="60\%"}
#'
#' @seealso \code{\link{ic.diferencia.proporciones}}
#'
#' @references
#' Casas José M. () Inferencia estadística. Editoral: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
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

      print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9) y unos (presencia/\u00e9)")
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
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z2,-estadistico.Z2,-valor_critico,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  } else if(tipo_contraste == "cola derecha"){

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
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  } else{

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
        labs(x = "z", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(estadistico.Z,-valor_critico)) +
        theme(axis.text.x = element_text(angle = 45))
    }

  }


  CH <- cbind(H0,estadistico.Z,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  if(isTRUE(grafico)){

    return(list(CH,plot))

  } else{

    return(CH)

  }

}


