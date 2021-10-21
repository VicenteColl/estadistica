#' @title Contraste de hipótesis sobre la diferencia de medias.
#'
#' @description Realiza el contraste de hipótesis sobre la diferencia de medias poblacionales.
#'
#' @usage contraste.diferencia.medias(x,
#'                  variable = NULL,
#'                  introducir = FALSE,
#'                  var_pob = c("conocida","desconocida"),
#'                  iguales = FALSE,
#'                  hipotesis_nula = 0,
#'                  tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
#'                  alfa = 0.05,
#'                  grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param var_pob Es un carácter. Indica si la varianza poblacional es conocida (por defecto, var_pob = "conocida") o desconocida. En este último caso debería cambiarse el argumento a var_pob = "desconocida".
#' @param iguales Si las varianzas poblacionales se consideran distintas (por defecto iguales = FALSE) o iguales (cambiar el argumento a iguales = TRUE).
#' @param hipotesis_nula Es un valor numérico.
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
#' @seealso \code{\link{ic.diferencia.medias}}
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
contraste.diferencia.medias <- function(x,
                                        variable = NULL,
                                        introducir = FALSE,
                                        var_pob = c("conocida","desconocida"),
                                        iguales = FALSE,
                                        hipotesis_nula = 0,
                                        tipo_contraste = c("bilateral","cola derecha","cola izquierda"),
                                        alfa = 0.05,
                                        grafico = FALSE){


  tipo_contraste <- tolower(tipo_contraste)
  tipo_contraste <- match.arg(tipo_contraste)

  var_pob <- tolower(var_pob)
  var_pob <- match.arg(var_pob)

  if(is.null(hipotesis_nula) | !is.numeric(hipotesis_nula)){

    stop("Tienes que introducir un valor para la hipotesis nula")

  } else{

    H0 <- hipotesis_nula

  }


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){

      x <- x

    } else{

      warning("Para calcular el intervalo de confianza hay que seleccionar una variable")
      stop("El conjunto de datos seleccionado tiene mas de 1 variables.")

    }

  } else if(length(variable) == 2){

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

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

    warning("Para calcular el intervalo de confianza hay que seleccionar dos variables")
    stop("El conjunto de datos seleccionado no es adecuado.")

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1os de la muestras
  n1 <- length(x[1][!is.na(x[1])])
  n2 <- length(x[2][!is.na(x[2])])
  n <- c(n1,n2)

  # medias muestrales
  media1 <- as.numeric(media(x[1]))
  media2 <- as.numeric(media(x[2]))


  if(var_pob == "conocida"){

    if(isTRUE(iguales)){

      varianza_pob1 <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
      varianza_pob1 <- as.numeric(varianza_pob1)

      varianza_pob2 <- varianza_pob1

    } else{

      varianza_pob1 <- readline(prompt = "Introducir el valor de la varianza poblacional 1: ")
      varianza_pob1 <- as.numeric(varianza_pob1)

      varianza_pob2 <- readline(prompt = "Introducir el valor de la varianza poblacional 2: ")
      varianza_pob2 <- as.numeric(varianza_pob2)

    }

  } else{

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu1 = as.numeric(varianza(x[1]))
      var_mu2 = as.numeric(varianza(x[2]))


    } else{

      var_mu1 <- as.numeric(varianza(x[1],tipo="cuasi"))
      var_mu2 <- as.numeric(varianza(x[2],tipo="cuasi"))

    }

  }


} else{   # aqu\uf00ed empieza introducir datos

  n <- c()
  media <- c()

  for(i in 1:2){

    n0 <- readline(prompt = paste("Introducir el tama\u00f1o de la muestra ",i,": ",sep=""))
    n0 <- as.numeric(n0)

    n <- c(n,n0)

    media0 <- readline(prompt = paste("Introducir el valor de la media muestral ",i,": ",sep=""))
    media0 <- as.numeric(media0)

    media <- c(media,media0)

  }

    if(var_pob == "conocida"){

      varianza_pob <- c()

      if(isTRUE(iguales)){

        varianza_pob0 <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
        var_pob0 <- as.numeric(varianza_pob0)

        varianza_pob <- c(var_pob0,var_pob0)


      } else{

        for(i in 1:2){

        varianza_pob0 <- readline(prompt = paste("Introducir el valor de la varianza poblacional ",i,": ",sep=""))
        var_pob0 <- as.numeric(varianza_pob0)

        varianza_pob <- c(varianza_pob,var_pob0)

        }
      }

    } else{ # las varianzas son desconocidas

      var_mu <- c()

      var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

      for(i in 1:2){

        if(var_muestra == 1){

          varianza_muestral0 <- readline(prompt = paste("Introducir el valor de la varianza muestral ",i,": ",sep=""))
          var_mu0 <- as.numeric(varianza_muestral0)

          var_mu <- c(var_mu, var_mu0)

        } else{

          varianza_cuasi0 <- readline(prompt = paste("Introducir el valor de la cuasivarianza muestral ",i,": ",sep=""))
          var_mu0 <- as.numeric(varianza_cuasi0)

          var_mu <- c(var_mu, var_mu0)

        }
      }
    }

  n1 <- n[1]
  n2 <- n[2]
  media1 <- media[1]
  media2 <- media[2]

  if(var_pob == "conocida"){
    varianza_pob1 <- varianza_pob[1]
    varianza_pob2 <- varianza_pob[2]

  } else{
    var_mu1 <- var_mu[1]
    var_mu2 <- var_mu[2]
  }

}


# calculo contrastes

  dif_medias <- media1 - media2

  if (n1 > 30 & n2 > 30){
    tamano <- "grande"
  } else {
    tamano <- "pequeno"
  }

  if(var_pob == "conocida"){

    # caso 1
    error_tipico <- sqrt(varianza_pob1/n1 + varianza_pob2/n2)

  } else{ # var poblacionales desconocidas

    if(tamano == "grande"){

      print("Como los tama\u00f1os muestrales son grandes, se aproximan las varianzas poblacionales por las varianzas (o cuasi-varianzas) muestrales")
      error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

    } else{  # tama\u00f1os muestrales peque\u00f1os

      if(isFALSE(iguales)){ # varianzas desconocidas y distintas

        if(var_muestra == 1){

          # varianzas poblacionales desconocidas y distintas (varianza muestral)
          numerador <- (var_mu1/(n1-1) + var_mu2/(n2-1))^2
          denominador <- ((var_mu1/(n1-1))^2/(n1+1))+((var_mu2/(n2-1))^2/(n2+1))
          gl <- (numerador / denominador)-2
          gl <- ceiling(gl)
          error_tipico <- sqrt((var_mu1/(n1-1))+(var_mu2/(n2-1)))

        } else{

          # varianzas poblacionales desconocidas y distintas (cuasivarianza muestral)
          print("Este es el contraste que generalmente calculan los softwares (SPSS, Excel, etc.)")
          numerador <- (var_mu1/n1 + var_mu2/n2)^2
          denominador <- ((var_mu1/n1)^2/(n1-1))+((var_mu2/n2)^2/(n2-1))
          gl <- numerador / denominador
          gl <- ceiling(gl)
          error_tipico <- sqrt((var_mu1/n1)+(var_mu2/n2))

        }

      } else{ # varianzas desconocida pero iguales

        gl <- n1+n2-2

        if(var_muestra == 1){

          # caso 2_1 con varianza muestral
          numerador <- sqrt(n1+n2) * sqrt(n1*var_mu1 + n2*var_mu2)
          denominador <- sqrt(n1*n2) * sqrt(n1+n2-2)

          error_tipico <- numerador/denominador

        } else{   # comprobado con la cuasi

          # caso 2_2 con cuasivarianza muestral

          print("Este es el contraste que generalmente calculan los softwares (SPSS, Excel, etc.)")
          numerador <- var_mu1*(n1-1) + var_mu2*(n2-1)
          denominador <- n1+n2-2

          error_tipico <- sqrt(numerador/denominador)*sqrt((n1+n2)/(n1*n2))

        }

      }
    }
  }

  estadistico.prueba <- (dif_medias - H0)/error_tipico

  if(alfa >= 0 & alfa <=1){

    if(var_pob == "conocida" | (var_pob == "desconocida" & tamano == "grande")){

      if(tipo_contraste == "bilateral"){
        valor_critico <- qnorm(alfa/2,lower.tail = F)
      }
      if(tipo_contraste == "cola izquierda"){
        valor_critico <- qnorm(alfa,lower.tail = T)
      }
      if(tipo_contraste == "cola derecha"){
        valor_critico <- qnorm(alfa,lower.tail = F)
      }

    } else{

      if(tipo_contraste == "bilateral"){
        valor_critico <- qt(alfa/2,gl,lower.tail = F)
      }
      if(tipo_contraste == "cola izquierda"){
        valor_critico <- qt(alfa,gl,lower.tail = T)
      }
      if(tipo_contraste == "cola derecha"){
        valor_critico <- qt(alfa,gl,lower.tail = F)
      }

    }

    valor_critico <- round(valor_critico,4)

  } else{
    stop("El nivel de significacion debe fijarse entre 0 y 1")
  }


  if(tipo_contraste == "bilateral"){

    estadistico.prueba2 <- abs(estadistico.prueba)
    media_inf <- H0 - valor_critico * error_tipico
    media_sup <- H0 + valor_critico * error_tipico

    if(var_pob == "conocida" | (var_pob == "desconocida" & tamano == "grande")){

      pvalor <- 2 * pnorm(estadistico.prueba2,lower.tail=FALSE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-4,4))) +
          geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, -valor_critico)) +
          geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
          geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 4)) +
          geom_vline(xintercept = -estadistico.prueba2, linetype = "dashed") +
          geom_vline(xintercept = estadistico.prueba2, linetype = "dashed") +
          labs(x = "", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba2,-estadistico.prueba2,-valor_critico,valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
        plot2 <- ggplot(df, aes(x)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(df[1,1], media_inf)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(media_inf, media_sup)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(media_sup, df[2,1])) +
          geom_vline(xintercept = dif_medias, linetype = "dashed") +
          labs(x = "", y = "",title="Intervalo de la diferencia de medias muestrales\n(supuesta H0 cierta)") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(media_inf,dif_medias,media_sup)) +
          theme(axis.text.x = element_text(angle = 45))


      }


    } else{

      pvalor <- 2 * pt(estadistico.prueba2,gl, lower.tail=FALSE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-3,3))) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(-3, -valor_critico)) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(valor_critico, 3)) +
          geom_vline(xintercept = 0, color = "black") +
          geom_vline(xintercept = -estadistico.prueba2, color = "blue", linetype = "dashed") +
          geom_vline(xintercept = estadistico.prueba2, color = "blue", linetype = "dashed") +
          labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(-estadistico.prueba2,estadistico.prueba2,valor_critico,-valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        plot2 <- NULL


      }

    }

    if(estadistico.prueba >= -valor_critico & estadistico.prueba <=  valor_critico){

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [", -valor_critico," , ",valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

  }

  if(tipo_contraste == "cola derecha"){

    media_inf <- -Inf
    media_sup <- H0 + valor_critico * error_tipico

    if(var_pob == "conocida" | (var_pob == "desconocida" & tamano == "grande")){

      pvalor <- pnorm(estadistico.prueba,lower.tail=FALSE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-3,3))) +
          geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(-3L,valor_critico)) +
          geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(valor_critico, 3L)) +
          geom_vline(xintercept = estadistico.prueba, linetype = "dashed") +
          labs(x = "", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba,valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
        plot2 <- ggplot(df, aes(x)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(df[1,1], media_sup)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(media_sup, df[2,1])) +
          geom_vline(xintercept = dif_medias, linetype = "dashed") +
          labs(x = "", y = "",title="Intervalo de la diferencia de medias muestrales\n(supuesta H0 cierta)") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(dif_medias,media_sup)) +
          theme(axis.text.x = element_text(angle = 45))

      }


    } else{

      pvalor <- pt(estadistico.prueba,gl, lower.tail=FALSE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-4,4))) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(-4, valor_critico)) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(valor_critico, 4)) +
          geom_vline(xintercept = 0, color = "black") +
          geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
          labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba,valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        plot2 <- NULL


      }


    }

    if(estadistico.prueba > valor_critico){

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo ]-Inf , ", valor_critico,"]",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }


  }

  if(tipo_contraste == "cola izquierda"){

    media_inf <- H0 + valor_critico * error_tipico # valor critico negativo
    media_sup <- Inf

    if(var_pob == "conocida" | (var_pob == "desconocida" & tamano == "grande")){

      pvalor <- pnorm(estadistico.prueba,lower.tail=TRUE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-4,4))) +
          geom_area(stat = "function", fun = dnorm, fill = "red", xlim = c(-4, valor_critico)) +
          geom_area(stat = "function", fun = dnorm, fill = "darkgreen", xlim = c(valor_critico, 4)) +
          geom_vline(xintercept = estadistico.prueba, linetype = "dashed") +
          labs(x = "z", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba,-valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        df <- data.frame(x = c(H0-4*error_tipico, H0+4*error_tipico))
        plot2 <- ggplot(df, aes(x)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "red", xlim = c(df[1,1], media_inf)) +
          geom_area(stat = "function", fun = dnorm, args= list(mean = H0, sd = error_tipico), fill = "darkgreen", xlim = c(media_inf, df[2,1])) +
          geom_vline(xintercept = dif_medias, linetype = "dashed") +
          labs(x = "", y = "",title="Intervalo de la diferencia de medias muestrales\n(supuesta H0 cierta)") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(media_inf,dif_medias)) +
          theme(axis.text.x = element_text(angle = 45))

      }

    } else{

      pvalor <- pt(estadistico.prueba, gl, lower.tail=TRUE)

      if(isTRUE(grafico)){

        plot <- ggplot(NULL, aes(c(-3,3))) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "darkgreen", xlim = c(valor_critico,3)) +
          geom_area(stat = "function", fun = dt, args = list(df = gl), fill = "red", xlim = c(-3,valor_critico)) +
          geom_vline(xintercept = 0, color = "black") +
          geom_vline(xintercept = estadistico.prueba, color = "blue", linetype = "dashed") +
          labs(title = paste("Distribuci\u00f3n t con ", gl, " grados de libertad",sep=""), x = "", y = "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = c(estadistico.prueba,valor_critico)) +
          theme(axis.text.x = element_text(angle = 45))

        plot2 <- NULL


      }

    }

    if(estadistico.prueba < valor_critico){

      print(paste("Se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) no se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    } else{

      print(paste("No se rechaza la hip\u00f3tesis nula. La regi\u00f3n de aceptaci\u00f3n viene dada por el intervalo [ ",valor_critico," , inf[",sep=""))
      print("El valor del estad\u00edstico de prueba (o valor experimental) se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n")

    }

  }


  CH <- cbind(H0,estadistico.prueba,pvalor)
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  Imedia <- cbind(`limite inferior`=media_inf,`limite superior`=media_sup)

  if(isTRUE(grafico)){

    grafico <- list(plot,plot2)

    return(list(`Estadistico`=CH,`Intervalo de la diferencia de medias muestrales (supuesta H0 cierta)`= Imedia,`Graficos`= grafico))

  } else{

    return(list(`Estadistico`=CH,`Intervalo de la diferencia de medias muestrales (supuesta H0 cierta)`= Imedia))

  }
}
