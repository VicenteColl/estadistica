ic.diferencia.medias <- function(x,
                                 variable = NULL,
                                 introducir = FALSE,
                                 distribucion = c("normal","desconocida"),
                                 var_pob = c("conocida","desconocida"),
                                 iguales = FALSE,
                                 confianza = 0.95){


  source("./R/varianza.R")
  source("./R/media.R")

  distribucion <- tolower(distribucion)
  distribucion <- match.arg(distribucion)

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

  # tamaños de la muestras
  n1 <- length(x[1][!is.na(x[1])])
  n2 <- length(x[2][!is.na(x[2])])
  n <- c(n1,n2)

  # medias muestrales
  media1 <- media(x[1])
  media2 <- media(x[2])


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

      var_mu1 = varianza(x[1])
      var_mu2 = varianza(x[2])


    } else{

      var_mu1 <- varianza(x[1],tipo="cuasi")
      var_mu2 <- varianza(x[2],tipo="cuasi")

    }

  }


} else{   # aquí empieza introducir datos

  n <- c()
  media <- c()

  for(i in 1:2){

    n0 <- readline(prompt = paste("Introducir el tamaño de la muestra ",i,": ",sep=""))
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

# calculo intervalos

  dif_medias <- media1 - media2

  if(distribucion == "normal"){

    if(var_pob == "conocida"){

      # caso de varianza poblacionales conocidas (iguales o distintas por la seleccion de varianza poblacional)
      # casos 1 y 2
      valor_critico <- qnorm(alfa2,lower.tail = FALSE)
      error_tipico <- sqrt(varianza_pob1/n1 + varianza_pob2/n2)


    } else{ # varianzas poblacionales desconocidas

      if(isTRUE(iguales)){ # varianzas poblaciones desconocidas e iguales

        if(all(n>=30)){ # muestras grandes

          print("Si los tamañas muestrales son grandes se estima la varianza poblacional")
          valor_critico <- qnorm(alfa2,lower.tail = FALSE)
          error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

        } else{ # muestras pequeñas

          # caso 3. n1 y n2 son pequeñas

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

        # caso 4 (solo con cuasivarianza muestral)
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

  } else{ # distribución es desconocida

    if(var_pob == "conocida"){


    } else{ # distribucion desconocida y varianzas poblaciones desconocidas

      if(n1 >= 30 & n2 >= 30){ # muestras grandes

        # según libro de casas

        valor_critico <- qnorm(alfa2,lower.tail = FALSE)

        if(isTRUE(iguales)){

          # caso 5
          # error tipico igual al del caso 3_2 pero el valor crítico con normal
          numerador <- var_mu1*(n1-1) + var_mu2*(n2-1)
          denominador <- n1+n2-2

          error_tipico <- sqrt(numerador/denominador)*sqrt((n1+n2)/(n1*n2))

        } else {

          # caso 6
          # error tipico como el caso 1 y 2 pero con las varianzas poblaciones estimadas con las muestrales
          error_tipico <- sqrt(var_mu1/n1 + var_mu2/n2)

        }

      } else{ #muestras pequeñas

        print("La distribución de probabilidad de la población y su varianza son desconocidas. Además, el tamaño de alguna muestra es pequeño (n<30)")
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
