covarianza <- function(x, variable = NULL){

  if(is.null(variable)){

    if(length(x) == 2){

      x <- data.frame(x)
      varnames <- names(x)

    } else{

      warning("Para obtener la matriz de correlacion utilizar la funcion matriz.cor")
      stop("El conjunto de datos seleccionado tiene mas de 2 variables.")

    }

  } else if(length(variable) == 2){

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable

      } else{

        stop("Seleccion errronea de variables")

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
    varnames <- names(x)

  } else{

    warning("Para obtener la matriz de correlacion utilizar la funcion matriz.cor")
    stop("Para calcular la correlacion solo puedes seleccionar dos variables")

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse la covarianza, alguna variable que has seleccionado no es cuantitativa")

  }

  covarianza <- cov(x[1],x[2], use ="everything")
  covarianza <- as.data.frame(covarianza)

  names(covarianza) <- paste("covarianza_",varnames[1],"_",varnames[2],sep="")
  row.names(covarianza) <- NULL

  return(covarianza)

}
