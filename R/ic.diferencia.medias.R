#' @title Intervalo confianza para la diferencia de medias.
#'
#' @description Calcula el intervalo de confianza de la diferencia de medias poblacionales.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qricdiferenciamedias.png}{options: width="25\%" alt="qricdiferenciamedias.png"}}
#' \if{latex}{\figure{qricdiferenciamedias.png}{options: width=3cm}}
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere a dos variables, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre tamaño muestral, valor de la media muestral, etc.
#' @param poblacion Es un carácter. Indica la distribución de probabilidad de la población. Por defecto \code{poblacion = "normal"}. Si la distribución de la población es desconocida, cambiar a \code{poblacion = "desconocida"}.
#' @param var_pob Es un carácter. Indica si la varianza poblacional es conocida (por defecto, \code{var_pob = "conocida"}) o desconocida. En este último caso debería cambiarse el argumento a \code{var_pob = "desconocida"}.
#' @param iguales Por defecto se considera que las varianzas poblacionales son distintas (\code{iguales = FALSE}). En el supuesto de varianzas poblacionales iguales cambiar el argumento a \code{iguales = TRUE}.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento)
#'
#' @return Devuelve el intervalo de confianza de la diferencia de medias poblacionales en un objeto de tipo \code{data.frame}.
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
#' Se obtienen los intervalos según los siguientes casos:
#'
#' Caso 1: Varianzas poblacionales conocidas
#'
#' \if{html}{\figure{icdifmedias1.png}{options: width="75\%" alt="icdifmedias1.png"}}
#' \if{latex}{\figure{icdifmedias1.png}{options: width=10cm}}
#'
#' Nota: Si los tamaños muestrales nx y ny son suficientemente grandes, pueden estimarse las varianzas poblacionales
#' por sus correspondientes varianzas (o cuasivarianzas), incluso aunque las distribuciones poblacionales no sean normales
#' (por aplicación del TCL).
#'
#' Caso 2. Varianzas poblacionales desconocidas pero iguales
#'
#' (2.1) con varianza muestral:
#'
#' \if{html}{\figure{icdifmedias2.png}{options: width="70\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias2.png}{options: width=10cm}}
#'
#' (2.2) con cuasivarianza muestral:
#'
#' \if{html}{\figure{icdifmedias2cuasi.png}{options: width="75\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias2cuasi.png}{options: width=10cm}}
#'
#' Nota: Tanto en el caso (2.1) como (2.2) la distribución t tiene (nx+ny-2) grados de libertad.
#'
#' Caso 3. Varianzas poblacionales desconocidas y distintas
#'
#' (3.1) con varianza muestral:
#'
#' \if{html}{\figure{icdifmedias3.png}{options: width="60\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias3.png}{options: width=8cm}}
#'
#'la distribución t con grados de libertad igual al entero más próximo de v.
#'
#' \if{html}{\figure{icdifmedias3gl.png}{options: width="65\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias3gl.png}{options: width=8cm}}
#'
#' (3.2) con cuasivarianza muestral:
#'
#' \if{html}{\figure{icdifmedias3cuasi.png}{options: width="60\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias3cuasi.png}{options: width=7cm}}
#'
#' la distribución t con grados de libertad igual a v, donde v = (parte entera de v*) + 1
#'
#' \if{html}{\figure{icdifmedias3cuasigl.png}{options: width="65\%" alt="icdifproporciones.png"}}
#' \if{latex}{\figure{icdifmedias3cuasigl.png}{options: width=6cm}}
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
#' @importFrom stats pnorm qnorm pt qt na.omit
#' @import dplyr ggplot2
#'
#' @export
ic.diferencia.medias <- function(x,
                                 variable = NULL,
                                 introducir = FALSE,
                                 poblacion = c("normal","desconocida"),
                                 var_pob = c("conocida","desconocida"),
                                 iguales = FALSE,
                                 confianza = 0.95){


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

    warning("Para calcular el intervalo de confianza hay que seleccionar dos variables")
    stop("El conjunto de datos seleccionado no es adecuado.")

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse el intervalo de confianza porque la variable seleccionada no es cuantitativa")

  }

  # tama\i00f1os de la muestras
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

    var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

    if(var_muestra == 1){

      var_mu1 = as.numeric(varianza(x[1]))
      var_mu2 = as.numeric(varianza(x[2]))


    } else{

      var_mu1 <- as.numeric(varianza(x[1],tipo="cuasi"))
      var_mu2 <- as.numeric(varianza(x[2],tipo="cuasi"))

    }

  }


} else{   # aqu\u00ed empieza introducir datos

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

      var_muestra <- as.numeric(readline('Selecciona el valor que quieres utilizar: \n 1. "Varianza muestral" \n 2. "Cuasivarianza muestral" \n'))

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

# calculo intervalos

  dif_medias <- media1 - media2

  if(poblacion == "normal"){

    if(var_pob == "conocida"){

      # caso de varianza poblacionales conocidas (iguales o distintas por la seleccion de varianza poblacional)
      # casos 1 y 2
      valor_critico <- qnorm(alfa2,lower.tail = FALSE)
      error_tipico <- sqrt(varianza_pob1/n1 + varianza_pob2/n2)


    } else{ # varianzas poblacionales desconocidas

      if(isTRUE(iguales)){ # varianzas poblaciones desconocidas e iguales

        if(all(n>=30)){ # muestras grandes (caso 2)

          print("Como los tama\u00f1os muestrales son grandes se estima la varianza poblacional")
          valor_critico <- qnorm(alfa2,lower.tail = FALSE)
          error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

        } else{ # muestras peque\u00f1as

          # caso 3. n1 y n2 son peque\u00f1as

          valor_critico <- qt(alfa2,n1+n2-2,lower.tail = FALSE)

          if(var_muestra == 1){

            # caso 3_1 con varianza muestral
            numerador <- sqrt(n1+n2) * sqrt(n1*var_mu1 + n2*var_mu2)
            denominador <- sqrt(n1*n2) * sqrt(n1+n2-2)
            error_tipico <- numerador/denominador

          } else{   # comprobado con la cuasi

            # caso 3_2 con cuasivarianza muestral

            print("Este es el intervalo que generalmente calculan los softwares (SPSS, Excel, etc.)")
            numerador <- var_mu1*(n1-1) + var_mu2*(n2-1)
            denominador <- n1+n2-2

            error_tipico <- sqrt(numerador/denominador)*sqrt((n1+n2)/(n1*n2))

          }

        }

      } else {  # varianzas poblaciones desconocidas y distintas

        if(all(n>=30)){ # muestras grandes (caso 2)

          print("Como los tama\u00f1os muestrales son grandes se estima la varianza poblacional")
          valor_critico <- qnorm(alfa2,lower.tail = FALSE)
          error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

        } else{ # muestras pequenas (caso 4)

          if(var_muestra == 1){
          # caso 4.1
          # varianzas poblacionales desconocidas y distintas (varianza muestral)
          numerador <- (var_mu1/(n1-1) + var_mu2/(n2-1))^2
          denominador <- ((var_mu1/(n1-1))^2/(n1+1))+((var_mu2/(n2-1))^2/(n2+1))
          gl <- (numerador / denominador)-2
          gl <- ceiling(gl)
          valor_critico <- qt(alfa2,gl,lower.tail = FALSE)
          error_tipico <- sqrt((var_mu1/(n1-1))+(var_mu2/(n2-1)))

          } else{
          # caso 4.2
          # varianzas poblacionales desconocidas y distintas (cuasivarianza muestral)
          print("Este es el intervalo que generalmente calculan los softwares (SPSS, Excel, etc.)")
          numerador <- (var_mu1/n1 + var_mu2/n2)^2
          denominador <- ((var_mu1/n1)^2/(n1-1))+((var_mu2/n2)^2/(n2-1))
          gl <- numerador / denominador
          gl <- ceiling(gl)
          valor_critico <- qt(alfa2,gl,lower.tail = FALSE)
          error_tipico <- sqrt((var_mu1/n1)+(var_mu2/n2))

          }
        }
      }
    }

  } else{ # distribuci\u00f3n es desconocida

    if(var_pob == "conocida"){

      if(n1 >= 30 & n2 >= 30){ # muestras grandes

        print("Como los tama\u00f1os muestrales son grandes, por aplicaci\u00f3n del TCL se puede suponer que las medias muestrales se distribuyen aproximadamente normales")

        valor_critico <- qnorm(alfa2,lower.tail = FALSE)
        error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

      } else {

        print("La distribuci\u00f3n de probabilidad de la poblaci\u00f3n es desconocida y el tama\u00f1o de alguna muestra es peque\u00f1o (n<30)")
        stop("Bajo estas condiciones no es posible estimar el intervalo de confianza")

      }


    } else{ # poblacion desconocida y varianzas poblaciones desconocidas

      if(n1 >= 30 & n2 >= 30){ # muestras grandes

        # se\u00fan libro de casas

        valor_critico <- qnorm(alfa2,lower.tail = FALSE) # valor critico normal

        if(isTRUE(iguales)){

          if(var_muestra == 1){

            # similar caso 3_1 con varianza muestral
            numerador <- sqrt(n1+n2) * sqrt(n1*var_mu1 + n2*var_mu2)
            denominador <- sqrt(n1*n2) * sqrt(n1+n2-2)
            error_tipico <- numerador/denominador

          } else{   # comprobado con la cuasi

            # similar caso 3_2 con cuasivarianza muestral

            print("Este es el intervalo que generalmente calculan los softwares (SPSS, Excel, etc.)")
            numerador <- var_mu1*(n1-1) + var_mu2*(n2-1)
            denominador <- n1+n2-2

            error_tipico <- sqrt(numerador/denominador)*sqrt((n1+n2)/(n1*n2))

          }


        } else {

          if(var_muestra == 1){

            # similar caso 4_1 con varianza muestral (se estima var_pob por varianza muestral)
            error_tipico <- sqrt((var_mu1/n1)+(var_mu2/n2))

          } else{   # comprobado con la cuasi (se estima var_pob por estimador insesgado: cuasivar muestral)

            # similar caso 4_2 con cuasivarianza muestral
            error_tipico <- sqrt((var_mu1/n1)+(var_mu2/n2))

          }

        }

      } else{ #muestras peque\u00f1as

        print("La distribuci\u00f3n de probabilidad de la poblaci\u00f3 y su varianza son desconocidas. Adem\u00e1s, el tama\u00f1o de alguna muestra es peque\u00f1o (n<30)")
        stop("Bajo estas condiciones no es posible estimar el intervalo de confianza")


      }

    }

  }

  limite_inferior <- dif_medias - valor_critico * error_tipico
  limite_superior <- dif_medias + valor_critico * error_tipico


  IC <- cbind(limite_inferior,limite_superior)
  IC <- as.data.frame(IC)
  row.names(IC) <- NULL

  return(IC)

}
