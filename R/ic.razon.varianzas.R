#' @title Intervalo confianza para la razón (cociente) de varianzas.
#'
#' @description Calcula el intervalo de confianza para la razón (o cociente) de varianzas.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qricrazonvarianzas.png}{options: style="width: 25\%;" alt="Figure: qricrazonvarianzas.png"}}
#' \if{latex}{\figure{qricrazonvarianzas.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere a dos variables, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param media_pob Es un carácter. Por defecto se supone que la media poblacional es desconocida (\code{media_pob="desconocida"})
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
#'
#' @return Devuelve el intervalo del cociente de varianzas poblacionales en un objeto de tipo \code{data.frame}. Si \code{grafico = T} devuelve una \code{list} con el intervalo de confianza y su representación gráfica.
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
#' Esta función calcula el intervalo de confianza para el cociente entre la varianza poblacional de la muestra 1 y la de la muestra 2, es decir:
#'
#' \if{html}{\figure{iccocientevar.png}{options: style="width: 6\%;" alt="Figure: iccocientevar.png"}}
#' \if{latex}{\figure{iccocientevar.png}{options: width=1cm}}
#'
#' Para obtener los intervalos de confianza se opera sobre el estadístico F que se facilita en la nota y que se utiliza para obtener el intervalo del cociente de la varianza de la muestra 2 y la muestra 1.
#'
#' Los intervalos se obtienen bajo el supuesto de que la media poblacional es desconocida:
#'
#' (1) si se trabaja con las varianzas muestrales
#'
#' \if{html}{\figure{iccocientevarmuestra.png}{options: style="width: 110\%;" alt="Figure: iccocientevarmuestra.png"}}
#' \if{latex}{\figure{iccocientevarmuestra.png}{options: width=14cm}}
#'
#' (2) si se trabaja con las cuasi-varianzas muestrales
#'
#' \if{html}{\figure{iccocientevarcuasi.png}{options: style="width: 80\%;" alt="Figure: iccocientevarcuasi.png"}}
#' \if{latex}{\figure{iccocientevarcuasi.png}{options: width=14cm}}
#'
#'
#' @note
#' En el caso de querer deducir el intervalo recíproco, es decir:
#'
#' \if{html}{\figure{iccocientevar2.png}{options: style="width: 6\%;" alt="Figure: iccocientevar.png"}}
#' \if{latex}{\figure{iccocientevar2.png}{options: width=1cm}}
#'
#' se parte del estadístico:
#'
#' \if{html}{\figure{Frazonvar.png}{options: style="width: 50\%;" alt="Figure: iccocientevar.png"}}
#' \if{latex}{\figure{Frazonvar.png}{options: width=8cm}}
#'
#' (3) si se trabaja con las varianzas muestrales
#'
#' \if{html}{\figure{iccocientevarmuestra2.png}{options: style="width: 110\%;" alt="Figure: iccocientevarmuestra.png"}}
#' \if{latex}{\figure{iccocientevarmuestra2.png}{options: width=14cm}}
#'
#' (4) si se trabaja con las cuasi-varianzas muestrales
#'
#' \if{html}{\figure{iccocientevarcuasi2.png}{options: style="width: 80\%;" alt="Figure: iccocientevarcuasi.png"}}
#' \if{latex}{\figure{iccocientevarcuasi2.png}{options: width=13cm}}
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
#' @importFrom stats pf qf df na.omit
#' @import dplyr ggplot2 cowplot
#'
#' @export
ic.razon.varianzas <- function(x,
                               variable = NULL,
                               introducir = FALSE,
                               media_pob = c("desconocida","conocida"),
                               confianza = 0.95,
                               grafico = FALSE){


  media_pob <- tolower(media_pob)
  media_pob <- match.arg(media_pob)

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

    var_mu1 <- as.numeric(varianza(x[1]))
    var_mu2 <- as.numeric(varianza(x[2]))

  } else{

    var_mu1 <- as.numeric(varianza(x[1], tipo = "cuasi"))
    var_mu2 <- as.numeric(varianza(x[2], tipo = "cuasi"))

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

    } else{

      var_mu1 <- readline("Introduce el valor de la cuasivarianza muestral 1: ")
      var_mu1 <- as.numeric(var_mu1)

      var_mu2 <- readline("Introduce el valor de la cuasivarianza muestral 2: ")
      var_mu2 <- as.numeric(var_mu2)

    }

}

# calculo del intervalo de confianza

  # IC del cociente de varianzas con medias desconocidas

  if(media_pob == "desconocida"){

    print("Se calcula el intervalo de confianza para el cociente de varianzas supuestas desconocidas las medias poblacionales")

    valor_critico1 <- qf(1-alfa_2, df1= n1-1, df2 = n2-1, lower.tail = F)
    valor_critico2 <- qf(alfa_2, df1= n1-1, df2 = n2-1, lower.tail = F)

    if(var_muestra == 1){

      # caso 1.1

      limite_inferior <- (n1/(n1-1))*((n2-1)/n2)*(var_mu1/var_mu2) * (1/valor_critico2)
      limite_superior <- (n1/(n1-1))*((n2-1)/n2)*(var_mu1/var_mu2) * (1/valor_critico1)

    } else {

      # caso 1.2
      print("Este es el intervalo de confianza que generalmente calculan los softwares (SPSS, Excel, Stata, etc.)")

      limite_inferior <- (var_mu1/var_mu2) * (1/valor_critico2)
      limite_superior <- (var_mu1/var_mu2) * (1/valor_critico1)

    }

  } else {
    # las medias poblacionales conocidas

    valor_critico1 <- qf(1-alfa_2, df1= n1, df2 = n2, lower.tail = F)
    valor_critico2 <- qf(alfa_2, df1= n1, df2 = n2, lower.tail = F)

    if(var_muestra == 1){

      stop("Lo sentimos, este caso a\u00fan no est\u00e1 implementado")

    } else {

      factor1 <- (n1-1)/n1
      factor2 <- (n2-1)/n2

      limite_inferior <- ((factor1*var_mu1)/(factor2*var_mu2)) * valor_critico1
      limite_superior <- ((factor1*var_mu1)/(factor2*var_mu2))  * valor_critico2

    }

  }

  if(grafico){

      percentil99 <- qf(.9999, df1= n1-1, df2 = n2-1)

      data <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
      data$y <-df(data$x, df1= n1-1, df2 = n2-1)

      plot1 <- ggplot(data, aes(x,y)) +
        geom_area(fill="darkgreen") +
        geom_area(data=subset(data,x<valor_critico1), fill = "grey") +
        geom_area(data=subset(data,x>valor_critico2),fill = "grey") +
        geom_vline(xintercept = 0L, color = "black") +
        labs(title = paste("Distribuci\u00f3n F con ", n1-1, " y ",n2-1," grados de libertad",sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = c(round(0L,0),round(valor_critico1,3),round(valor_critico2,3))) +
        theme(axis.text.x = element_text(angle = 45)) +
        geom_point(aes(x= valor_critico1 , y=0), color = "red", size = 3) +
        geom_point(aes(x= valor_critico2 , y=0), color = "blue", size = 3)

      intervalo <- data.frame(ic = round(c(inferior=limite_inferior,superior=limite_superior),4),y=c(0,0))

      plot2 <- ggplot(intervalo,aes(x= ic,y)) +
        geom_line(aes(group = y), color = "grey",size = 3)+
        geom_point(aes(color=ic), size=3,show.legend = FALSE) +
        geom_text(aes(label = ic), size = 2.5, vjust=2) +
        scale_y_continuous(expand=c(0,0)) +
        scale_color_gradientn(colours=c("red","blue"))+
        labs(y="",x="Intervalo de confianza del cociente de varianzas") +
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
