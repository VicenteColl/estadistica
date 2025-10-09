#' @title Intervalo confianza para la varianza.
#'
#' @description Calcula el intervalo de confianza de la varianza poblacional.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qricvarianza.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qricvarianza.png}{options: width=3cm}}
#'
#' @usage ic.varianza(x,
#'            variable = NULL,
#'            introducir = FALSE,
#'            media_poblacion = c("desconocida","conocida"),
#'            confianza = 0.95,
#'            grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param media_poblacion Es un carácter. Indica si la media de la población es desconocida (por defecto, \code{media_poblacion = "desconocida"}) o conocida (en este caso, cambiar \code{media_poblacion = "conocida"}).
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return Devuelve el intervalo de confianza de la varianza poblacional en un objeto de tipo \code{data.frame}. Si \code{grafico = T} devuelve una \code{list} con el intervalo de confianza y su representación gráfica.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' (1) Si la media poblacional es conocida:
#'
#' \if{html}{\figure{icvarianzamedcon.png}{options: width="50\%" alt="Figure: icvarianzamedcon.png"}}
#' \if{latex}{\figure{icvarianzamedcon.png}{options: width=8cm}}
#'
#' (2) Si la media poblacional es desconocida.
#'
#' Con la varianza muestral:
#'
#' \if{html}{\figure{icvarianzameddescmuestra.png}{options: width="30\%" alt="Figure: icvarianzameddescmuestra.png"}}
#' \if{latex}{\figure{icvarianzameddescmuestra.png}{options: width=6cm}}
#'
#' Con la cuasivarianza muestral:
#'
#' \if{html}{\figure{icvarianzameddesccuasi.png}{options: width="50\%" alt="Figure: icvarianzameddesccuasi.png"}}
#' \if{latex}{\figure{icvarianzameddesccuasi.png}{options: width=8cm}}
#'
#' Nota: En todos los casos se obtiene el valor de la chi-dos con n grados de libertad que deja a su derecha una probabilidad de alfa y 1-alfa.
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
#' @importFrom stats pchisq qchisq na.omit
#' @import dplyr ggplot2 cowplot
#'
#' @export
ic.varianza <- function(x,
                        variable = NULL,
                        introducir = FALSE,
                        media_poblacion = c("desconocida","conocida"),
                        confianza = 0.95,
                        grafico = FALSE){

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
  print("Intervalo de confianza para la varianza poblacional, supuesta desconocida la media poblacional.")

  valor_critico1 <- qchisq(alfa_2,lower.tail = F, df= gl)
  valor_critico2 <- qchisq(1-alfa_2, lower.tail = F, df= gl)

  limite_inferior <- (n * var_mu) / valor_critico1
  limite_superior <- (n * var_mu) / valor_critico2

} else{

  # caso 2. Media poblacional conocida, n peque\u00f1a
  print("Intervalo de confianza para la varianza poblacional, supuesta conocida la media poblacional. n peque\u00f1a")
  gl <- gl +1
  valor_critico1 <- qchisq(alfa_2,lower.tail = F, df= gl)
  valor_critico2 <- qchisq(1-alfa_2, lower.tail = F, df= gl)

  limite_inferior <- sumatorio / valor_critico1
  limite_superior <- sumatorio / valor_critico2

}

  if(grafico){

    percentil99 <- qchisq(.9999, gl)

    df <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
    df$y <-dchisq(df$x, gl)

    plot1 <- ggplot(df) +
      geom_path(aes(x,y))+
      geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "darkgreen", xlim = c(valor_critico2, valor_critico1)) +
      geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "grey", xlim = c(0, valor_critico2)) +
      geom_area(stat = "function", fun = dchisq, args = list(df = gl), fill = "grey", xlim = c(valor_critico1, percentil99)) +
      geom_vline(xintercept = 0, color = "black") +
      labs(title = paste("Distribuci\u00f3n chi con ", gl, " grados de libertad",sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = c(0L,round(valor_critico2,4),round(valor_critico1,4))) +
      theme(axis.text.x = element_text(angle = 45))+
      geom_point(aes(x= valor_critico2 , y=0), color = "orange4", size = 3) +
      geom_point(aes(x= valor_critico1 , y=0), color = "lightblue4", size = 3)

    intervalo <- data.frame(ic = round(c(inferior=limite_inferior,superior=limite_superior),4),y=c(0,0))

    plot2 <- ggplot(intervalo,aes(x= ic,y)) +
      geom_line(aes(group = y), color = "grey",size = 3)+
      geom_point(aes(color=ic), size=3,show.legend = FALSE) +
      geom_text(aes(label = ic), size = 2.5, vjust=2) +
      scale_y_continuous(expand=c(0,0)) +
      scale_color_gradientn(colours=c("red","blue"))+
      labs(y="",x="Intervalo de confianza") +
      tema_blanco

    # plot <- grid::grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
    plot <- cowplot::plot_grid(plot1, plot2, ncol = 1, align = "v", rel_heights = c(1, 1))

  }


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  if(grafico){

    return(list(IC,plot))

  } else{

    return(IC)

  }


}
