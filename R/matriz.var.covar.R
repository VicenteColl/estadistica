matriz.covar <- function(x, variable = NULL, tipo = "poblacional"){

  if(is.null(variable)){

    x <- data.frame(x)
    varnames <- names(x)

  } else{

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

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la varianza, alguna variable que has seleccionado no es cuantitativa")
  }

  tipo_covar <- c("poblacional","muestral")

  tipo <- tolower(tipo)

  if(!(tipo %in% tipo_covar)){

    stop("¿Quieres calcular la matriz de var-covar poblacional o muestral?")

  }

  if(tipo == "poblacional"){

    n <- nrow(x)
    factor = (n-1)/n

  } else{

    factor <- 1
  }

  matriz_covar <- factor * var(x, na.rm = TRUE) %>%
    as.data.frame()
  names(matriz_covar) <- varnames
  row.names(matriz_covar) <- varnames

  return(matriz_covar)

}


