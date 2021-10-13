#' @title Intervalo confianza para la diferencia de dos proporciones.
#'
#' @description Calcula el intervalo de confianza de la diferencia de dos proporciones.
#'
#' \figure{qr_ic.diferencia.proporciones.png}{options: align= "center" width="25\%" heigth="25\%"}
#'
#' @usage ic.diferencia.proporciones(x,
#' variable = NULL,
#' introducir = FALSE,
#' confianza = 0.95,
#' grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.#'
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
#' Se obtiene el intervalo:
#'
#' \figure{ic_dif_proporciones.png}{options: width="65\%" heigth="65\%"}
#'
#' Nota: Las proporciones muestrales del error típico son sustituidas por sus estimaciones máximo-verosímiles (proporciones muestrales).
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
#' @import dplyr
#'
#' @export
ic.diferencia.proporciones <- function(x,
                                       variable = NULL,
                                       introducir = FALSE,
                                       confianza = 0.95,
                                       grafico = FALSE){


if(isFALSE(introducir)) {

  x <- data.frame(x)
  varnames <- names(x)

  if(is.null(variable)){

    if(length(x) == 2){

      x <- x

    } else{

      warning("Para calcular el intervalo de confianza hay que seleccionar dos variables")
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

    warning("Para calcular el intervalo de confianza hay que seleccionar dos variables")
    stop("El conjunto de datos seleccionado no es adecuado.")

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse el intervalo de confianza porque las variables seleccionadas no son cuantitativas")

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


  print("Primero vas a introducir los datos de la muestra 1 y a continuaci\u00fen introducir\u00e1s los datos de la muestra 2")
  print("Si los datos provienen de encuestas realizadas antes y despu\u00e9s de una determinada acci\u00f3n, introduce primero los datos de la encuesta realizada despu\u00e9s de dicha acci\u00f3n")

  n1 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 1: ")
  n1 <- as.numeric(n1)


  p_mu1 <- readline(prompt = "Introducir el valor de la proporci\u00f3n muestral 1: ")
  p_mu1 <- as.numeric(p_mu1)


  n2 <- readline(prompt = "Introducir el tama\u00f1o de la muestra 2: ")
  n2 <- as.numeric(n2)

  p_mu2 <- readline(prompt = "Introducir el valor de la proporci\u00f3n muestral 2: ")
  p_mu2 <- as.numeric(p_mu2)


}

  if(confianza >= 0 & confianza <=1){

    confianza <- confianza
    alfa_2 <- (1-confianza)/2
    valor_critico <- qnorm(alfa_2,lower.tail=FALSE)

  } else{

    stop("El nivel de confianza debe fijarse entre 0 y 1")

  }


  error_tipico <- sqrt((p_mu1 * (1-p_mu1))/n1 + (p_mu2 * (1-p_mu2))/n2)

  limite_inf <- (p_mu1 - p_mu2) - valor_critico * error_tipico
  limite_sup <- (p_mu1 - p_mu2) + valor_critico * error_tipico

  if(limite_inf < limite_sup){

    limite_inferior <- limite_inf
    limite_superior <- limite_sup

  } else{

    limite_inferior <- limite_sup
    limite_superior <- limite_inf
  }


  if(grafico){

      seq <- seq(-4,4,length=1000) * error_tipico + (p_mu1 - p_mu2)
      seq <- as.data.frame(seq)

      plot <- ggplot(seq, aes(seq)) +
        stat_function(fun = dnorm, args = list(mean = (p_mu1 - p_mu2), sd = error_tipico)) +
        geom_area(stat = "function", fun = dnorm, args = list(mean = (p_mu1 - p_mu2), sd = error_tipico), fill = "darkgreen", xlim = c(limite_inferior,limite_superior)) +
        labs(x = "", y = "",title = paste("Intervalo de confianza de la diferencia de proporciones\n(NC=",confianza*100,"%)")) +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = round(c(limite_inferior,(p_mu1 - p_mu2),limite_superior),4)) +
        tema_blanco +
        theme(axis.text.x = element_text(angle = 45)) +
        theme(axis.line.x = element_line(color = "black") )

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

