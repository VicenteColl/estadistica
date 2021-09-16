matriz.correlacion <- function(x, variable = NULL){

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


  matriz_cor <- cor(x) %>%
    as.data.frame()
  names(matriz_cor) <- varnames
  row.names(matriz_cor) <- varnames

  return(matriz_cor)

}


