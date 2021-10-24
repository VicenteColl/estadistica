#' @title Intervalo confianza de una proporción.
#'
#' @description Calcula el intervalo de confianza de una proporción.
#'
#' \figure{qr_ic.proporcion.png}{options: "center" width="25\%" heigth="25\%"}
#'
#' @usage ic.proporcion(x,
#'                      variable = NULL,
#'                      introducir = FALSE,
#'                      irrestricto = FALSE,
#'                      confianza = 0.95,
#'                      grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param irrestricto Es un valor lógico. Por defecto, irrectricto = FALSE. si se considera un muestreo irrectricto (extracción sin reemplazamiento), cambiar el argumento a irrestricto = TRUE.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.#'
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' (1) Para tamaños muestrales muy grandes:
#'
#'  \figure{ic_proporcion_3.png}{options: width="80\%" heigth="80\%"}
#'
#' (2) Para cualquier tamaño muestral puede obtenerse el intervalo \figure{ic_proporcion_1.png}{options: width="15\%" heigth="15\%"}, correspondiendo los valores a las raíces de:
#'
#'  \figure{ic_proporcion_2.png}{options: width="80\%" heigth="80\%"}
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
#' @importFrom stats pnorm qnorm na.omit
#' @import dplyr ggplot2
#'
#' @export
ic.proporcion <- function(x,
                          variable = NULL,
                          introducir = FALSE,
                          irrestricto = FALSE,
                          confianza = 0.95,
                          grafico = FALSE){


  print("Intervalo de confianza de una proporci\u00f3n. El tama\u00f1o de la muestra es grande.")

  if(confianza >= 0 & confianza <=1){

    confianza <- confianza
    alfa2 <- (1- confianza)/2
    valor_critico <- qnorm(alfa2,lower.tail = F)

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

        stop("Selecci\u00fn err\u00f3nea de variable")

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

  if(!all(x == 0 | x==1)){

    print("Aplica a tus datos la condici\u00f3n que debe cumplir la poblaci\u00f3n para transfomar los datos en ceros (ausencia/no \u00e9xito) y unos (presencia/\u00e9xito)")
    stop("Los valores en la muestra deben ser 0 y 1.")

  }

  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra
  n <- nrow(x)

  if(n < 30){
    stop("El tama\u00f1o de la muestra es peque\u00f1a, la aproximaci\u00f3n a la normal no es buena.")
  }

  # media muestral

    p_mu <- round(sum(x,na.rm=TRUE)/n,4)

} else{   # aqu\u00ed empieza introducir datos

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n <- as.numeric(n)

  if(n < 30){
    stop("El tama\u00f1o de la muestra es peque\u00f1a, la aproximaci\u00f3n a la normal no es buena.")
  }

    p_mu <- readline(prompt = "Introducir el valor de la proporci\u00f3n muestral: ")
    p_mu <- as.numeric(p_mu)

}


  if(isFALSE(irrestricto)){

    aproximacion <- as.numeric(readline('\u00bfQuieres aproximar el valor de p por la proporci\u00f3n muestral? \n 1. "S\u00ed" \n 2. "No" \n'))

    if(aproximacion == 1){

      print("Como n es suficientemente grande, se aproxima el valor de p poblacional por su estimaci\u00f3n puntual (p muestral)")

      error_tipico <- sqrt((p_mu * (1-p_mu))/n)
      limite_inferior <- p_mu - valor_critico * error_tipico
      limite_superior <- p_mu + valor_critico * error_tipico

    } else{

      aproximacion  <- 2

      print("Este criterio no tiene en cuenta el tama\u00f1o de la muestra. Se obtendr\u00e1 el intervalo de p a partir del c\u00e1lculo de probabilidad del estad\u00edstico")
      print("El intervalo obtenido no es sim\u00e9trico respecto a la proporci\u00f3n muestral")
      x <- n + valor_critico^2
      y <- -(2 * p_mu * n + valor_critico^2)
      z <- p_mu^2 * n

      limite_inf <- (-y + sqrt((y^2) - (4 * x * z)))/(2 * x)
      limite_sup <- (-y - sqrt((y^2) - (4 * x * z)))/(2 * x)

      if(limite_inf <= limite_sup){

        limite_inferior <- limite_inf
        limite_superior <- limite_sup

      } else{

        limite_inferior <- limite_sup
        limite_superior <- limite_inf

      }

    }

  } else{

    print("Intervalo para la proporci\u00f3n adecuado si el muestreo es sin reemplazamiento y la poblaci\u00f3n es finita")
    aproximacion <- 0
    N <- readline(prompt= "Introduce el tama\u00f1o (N) de la poblaci\u00f3n: ")
    N <- as.numeric(N)

    factor <- sqrt((N-n)/(N-1))
    error_tipico <- sqrt((p_mu * (1-p_mu))/n) * factor
    limite_inferior <- p_mu - valor_critico * error_tipico
    limite_superior <- p_mu + valor_critico * error_tipico

  }

  if(grafico){

    if(aproximacion == 2){

      intervalo <- data.frame(ic = round(c(inferior=limite_inferior,p_mu = p_mu, superior=limite_superior),4),y=c(0,0,0))

      plot <- ggplot(intervalo,aes(x= ic,y)) +
        geom_line(aes(group = y), color = "grey",size = 3)+
        geom_point(aes(color=ic), size=3,show.legend = FALSE) +
        geom_text(aes(label = ic), size = 2.5, vjust=2) +
        scale_y_continuous(expand=c(0,0)) +
        scale_color_gradientn(colours=c("red","darkgreen","blue"))+
        labs(y="",x="Intervalo de confianza de la proporci\u00f3n") +
        tema_blanco

    } else{

      seq <- seq(-4,4,length=1000) * error_tipico + p_mu
      seq <- as.data.frame(seq)

      plot <- ggplot(seq, aes(seq)) +
        stat_function(fun = dnorm, args = list(mean = p_mu, sd = error_tipico)) +
        geom_area(stat = "function", fun = dnorm, args = list(mean = p_mu, sd = error_tipico), fill = "darkgreen", xlim = c(limite_inferior,limite_superior)) +
        labs(x = "", y = "",title = paste("Intervalo de confianza de la proporci\u00f3n\n(NC=",confianza*100,"%)")) +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = round(c(limite_inferior,p_mu,limite_superior),4)) +
        tema_blanco +
        theme(axis.text.x = element_text(angle = 45)) +
        theme(axis.line.x = element_line(color = "black") )

    }

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

