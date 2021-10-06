#' @title Intervalo confianza para la media.
#'
#' @description Calcula el intervalo de confianza de la media poblacional.
#'
#' \figure{qr_ic.media.png}{options: "center" width="25\%" heigth="25\%"}
#'
#' @usage ic.media(x,
#'                 variable = NULL,
#'                 introducir = FALSE,
#'                 poblacion = c("normal","desconocida"),
#'                 var_pob = c("conocida","desconocida"),
#'                 confianza = 0.95,
#'                 grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si introducir = FALSE (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos x y/o variable. Si introducir = TRUE, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param poblacion Es un carácter. Indica la distribución de probabilidad de la población. Por defecto poblacion = "normal". Si la distribución de la población es desconocida, cambiar el argumento a poblacion = "desconocida".
#' @param var_pob Es un carácter. Indica si la varianza poblacional es conocida (por defecto, var_pob = "conocida") o desconocida. En este último caso debería cambiarse el argumento a var_pob = "desconocida".
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
#' @param grafico Es un valor lógico. Por defecto grafico = FALSE. Si se quiere obtener una representación gráfica del intervalo de confianza obtenido, cambiar el argumento a grafico = TRUE. Nota: Esta opción no está implementada para todos los casos.
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
#' (1) Si población desconocida, varianza poblacial conocida y muestra pequeña:
#'
#' \figure{ic_media_desconocida_var_con.png}{options: width="50\%" heigth="50\%"}
#'
#' (2) Si población normal, varianza poblacional conocida (muestra pequeña y grande)
#'
#' \figure{ic_media_var_con.png}{options: width="55\%" heigth="55\%"}
#'
#' (3) Si población normal, varianza poblacional desconocida y muestra pequeña
#'
#' Con la varianza muestral:
#'
#' \figure{ic_media_var_desc_muestral.png}{options: width="50\%" heigth="50\%"}
#'
#' Con la cuasivarianza muestral:
#'
#' \figure{ic_media_var_desc_cuasi.png}{options: width="50\%" heigth="50\%"}
#'
#' Nota: En ambos casos, el valor crítico sigue una distribución t con n-1 grados de libertad
#'
#' (4) Si población normal, varianza poblacional desconocida y muestra grande:
#' Puede utilizarse la aproximación a la normal. El intervalo se obtiene a partir de la expresión (2)
#' estimando la varianza poblacional por la varianza (o cuasivarianza) muestral.
#'
#' @references
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom ggalt geom_dumbbell
#' @import dplyr ggalt ggplot2 grid
#'
#' @export
ic.media <- function(x,
                     variable = NULL,
                     introducir = FALSE,
                     poblacion = c("normal","desconocida"),
                     var_pob = c("conocida","desconocida"),
                     confianza = 0.95,
                     grafico = FALSE){

poblacion <- tolower(poblacion)
poblacion <- match.arg(poblacion)

var_pob <- tolower(var_pob)
var_pob <- match.arg(var_pob)

if(confianza >= 0 & confianza <=1){

  confianza <- confianza
  alfa2 <- (1- confianza)/2

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
      stop("El conjunto de datos seleccionado tiene mas de 1 variables.")

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
    stop("El conjunto de datos seleccionado tiene mas de 1 variables.")

  }

  x <- na.omit(x)
  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\u00f1o de la muestra
  n <- nrow(x)

  # media muestral
  media <- media(x)

  if(var_pob == "conocida"){

    varianza_pob <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
    varianza_pob <- as.numeric(varianza_pob)
    desv_pob <- sqrt(varianza_pob)

  } else{   # varianza poblacional desconocida

      var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

      if(var_muestra == 1){

        desv_mu = desviacion(x)

      } else{

        desv_mu <- desviacion(x,tipo="cuasi")
      }

  }


} else{   # aqu\u00ed empieza introducir datos

  n <- readline(prompt = "Introducir el tama\u00f1o de la muestra: ")
  n <- as.numeric(n)

  # caso 1. No puede hacerse

  if(poblacion == "desconocida" & var_pob == "desconocida" & n < 30){

    print("La distribuci\u00f3n de probabilidad de la poblaci\u00f3n y su varianza son desconocidas. Adem\u00e1s, el tama\u00f1o de la muestra es peque\u00f1o (n<30)")
    stop("Bajo estas condiciones no es posible estimar el intervalo de confianza")

  }

  media <- readline(prompt = "Introducir el valor de la media muestral: ")
  media <- as.numeric(media)

  if(var_pob == "conocida"){
    varianza_pob <- readline(prompt = "Introducir el valor de la varianza poblacional: ")
    varianza_pob <- as.numeric(varianza_pob)
    desv_pob <- sqrt(varianza_pob)

  } else{ # varianza poblacional desconocida

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar? \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      varianza_muestral <- readline(prompt = "Introducir el valor de la varianza muestral: ")
      varianza_muestral <- as.numeric(varianza_muestral)
      desv_mu <- sqrt(varianza_muestral)

    } else{

      varianza_cuasi <- readline(prompt = "Introducir el valor de la cuasi-varianza muestral: ")
      varianza_cuasi <- as.numeric(varianza_cuasi)
      desv_mu <- sqrt(varianza_cuasi)
    }
  }
}

# calculo intervalos

  # caso 2 poblacion desconocida y varianza poblacional desconocida

  if(poblacion == "desconocida" & var_pob == "desconocida" & n >= 30){

    print("La distribuci\u00f3n de probabilidad de la poblaci\u00f3n y su varianza son desconocidaS. Sin embargo, el tama\u00f1o de la muestra es grande (n>30)")
    print("En esta situaci\u00f3n, la distribuci\u00f3in de la media muestral puede considerarse normal como consecuencia del TCL.")

    valor_critico <- qnorm(alfa2,lower.tail = FALSE)
    error_tipico <- desv_mu / sqrt(n)

  }

  # casos 3, 4: poblacion desconocida y varianza poblacional conocida

  if(poblacion == "desconocida" & var_pob == "conocida"){

    if(n < 30){

      print("Intervalo de confianza para la media poblacional supuesta conocida la varianza poblacional. n peque\u00f1a (n<30)")
      print("El intervalo de confianza ser\u00e1 obtenido a partir de Tchebychev")

      valor_critico <- 1
      error_tipico <- desv_pob / sqrt(n * alfa2 * 2)

    } else{

      print("Intervalo de confianza para la media poblacional supuesta conocida la varianza poblacional. n grande (n>30)")
      print("Como n es grande la distribuci\u00f3n de la media muestral puede considerarse normal como consecuencia del TCL.")

      valor_critico <- qnorm(alfa2,lower.tail = FALSE)
      error_tipico <- desv_pob / sqrt(n)

    }
  }


  if(poblacion == "normal"){

    # caso 5. Poblacion normal y varianza conocida

    if(var_pob == "conocida"){

      valor_critico <- qnorm(alfa2,lower.tail = FALSE)
      error_tipico <- desv_pob / sqrt(n)

    }

    if(var_pob == "desconocida"){

      if(n < 30){  # con la t-student

        if(var_muestra == 1){ # caso 6_1. normal con varianza desconocida muestra peque\uu00f1a (cuasivar muestral)
          valor_critico <- qt(alfa2,n-1,lower.tail = FALSE)
          error_tipico <- desv_mu /sqrt(n-1)

        } else{  # caso 6_1. normal con varianza desconocida muestra peque\u00f1a (var_muestral)
          print("Este es el intervalo que generalmente calculan los softwares (SPSS, Excel, etc.)")

          valor_critico <- qt(alfa2,n-1,lower.tail = FALSE)
          error_tipico <- desv_mu /sqrt(n)

        }

      }

      if(n >= 30){

        print("El tama\u00f1o de la muestra es grande (n>30). Se puede estimar la varianza poblacional y considerar el intervalo de varianza poblacional conocida.")
        aproximacion <- as.numeric(readline('\u00bfQuieres utilizar la aproximaci\u00f3n de la t a la normal? \n 1. "S\u00ed" \n 2. "No" \n'))

        if(aproximacion == 1){  # aproximacion normal. Se estima sigma por S
          # caso 7. Se estima sigma por S y se estima el IC del caso 5

          valor_critico <- qnorm(alfa2,lower.tail = FALSE)
          error_tipico <- desv_mu / sqrt(n)

        } else{  # con la t-student

          if(var_muestra == 1){ # caso 8_1 muestra grande, var muestral y t

            valor_critico <- qt(alfa2,n-1,lower.tail = FALSE)
            error_tipico <- desv_mu /sqrt(n-1)

          } else{  # caso 8_2 muestra grande, cuasi muestral y t

            valor_critico <- qt(alfa2,n-1,lower.tail = FALSE)
            error_tipico <- desv_mu /sqrt(n)

          }
        }
      }

    }

  }

  limite_inferior <- media - valor_critico * error_tipico
  limite_superior <- media + valor_critico * error_tipico

  if(isTRUE(grafico)){

    if(n>=30){
      tamano <- "grande"
    } else{
      tamano <- "peque\u00f1a"
    }

  tema_blanco <- theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
    axis.line.x = element_line(colour = "black"), # adding a black line for x and y axis
    axis.line.y = element_blank()
  )


  if(poblacion == "normal" & var_pob == "desconocida" & n<30){

    plot11 <- ggplot(NULL, aes(c(-4,4))) +
      geom_area(stat = "function", fun = dt, args = list(df = n-1), fill = "darkgreen", xlim = c(-valor_critico, valor_critico)) +
      geom_area(stat = "function", fun = dt, args = list(df = n-1), fill = "white", xlim = c(-valor_critico - 1, -valor_critico)) +
      geom_area(stat = "function", fun = dt, args = list(df = n-1), fill = "white", xlim = c(valor_critico, valor_critico + 1)) +
      labs(title = paste("Distribuci\u00f3n t con ", n-1, " grados de libertad",sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = c(-valor_critico,valor_critico)) +
      theme(axis.text.x = element_text(angle = 45)) +
      geom_point(aes(x= -valor_critico , y=0), color = "red", size = 3) +
      geom_point(aes(x= valor_critico , y=0), color = "blue", size = 3)


    intervalo <- data.frame(ic = "ic",inferior=limite_inferior,media=media,superior=limite_superior)
    plot12 <- ggplot(data = intervalo) +
      ggalt::geom_dumbbell(aes(y = ic,
                        x = inferior,
                        xend = superior),
                    size = 1.5,
                    color="#b2b2b2",
                    size_x=3,
                    size_xend = 3,
                    colour_x = "red",
                    colour_xend = "blue")  +
      ggalt::geom_dumbbell(aes(y = ic,
                        x = media,
                        xend = media),
                    size = 1.5,
                    size_x=3,
                    colour_x = "darkgreen") +
      labs(y="",x="") +
      geom_text(aes(y = ic, x=inferior, label=round(inferior,4)),
                color="black", size=3, vjust=2.5) +
      geom_text(aes(y = ic, x=superior, label=round(superior,4)),
                color="black", size=3, vjust=2.5) +
      geom_text(color="black", size=3, vjust=-2.5, hjust = +0.1,
                aes(y = ic, x=inferior, label="limite inferior"))+
      geom_text(aes(y = ic, x=superior, label="limite superior"),
                color="black", size=3, vjust=-2.5, hjust=+0.85) +
      geom_text(aes(y = ic, x=media, label="media"),
                color="black", size=3, vjust=-2.5) +
      geom_text(color="black", size=3, vjust=2.5,
                aes(y = ic, x=media, label=media)) +
      tema_blanco

    plot <- grid.draw(rbind(ggplotGrob(plot11), ggplotGrob(plot12), size = "first"))


  } else if(poblacion == "desconocida" & var_pob == "conocida" & n<30) {

    plot <- print("Este intervalo de confianza no tiene representaci\u00f3n gr\u00e1fica")

  } else{

    seq <- seq(-4,4,length=1000) * error_tipico + media
    seq <- as.data.frame(seq)

    plot <- ggplot(seq, aes(seq)) +
      stat_function(fun = dnorm, args = list(mean = media, sd = error_tipico)) +
      geom_area(stat = "function", fun = dnorm, args = list(mean = media, sd = error_tipico), fill = "darkgreen", xlim = c(limite_inferior,limite_superior)) +
      labs(x = "", y = "",title = paste("Intervalo de confianza de la media\n(poblaci\u00f3n",poblacion,",varianza poblacional",var_pob,",n",tamano,")\n(NC=",confianza*100,"%)")) +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = c(limite_inferior,media,limite_superior)) +
      theme(axis.text.x = element_text(angle = 45)) +
      tema_blanco
  }

  }

  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  if(isTRUE(grafico)){

    return(list(IC,plot))

  } else{

    return(IC)

  }

}
