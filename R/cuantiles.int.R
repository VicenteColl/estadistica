cuantiles.int <- function(x, pesos = NULL, cortes = 0.5){

  x <- data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse los cuantiles, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    x <- tidyr::drop_na(x)
    #y <- names(x)
    #names(x) <- varnames

    N <- nrow(x)
    tabla <- x %>% group_by(x) %>%
      count() %>%
      ungroup() %>%
      mutate(Ni = cumsum(n))

  } else{

    N <- sum(pesos)

    tabla <- data.frame(x,pesos) %>%
      na.omit
    #y <- names(tabla)
    names(tabla) <- c("x","pesos")

    tabla <- tabla %>%
      arrange(x) %>%
      mutate(Ni = cumsum(pesos))

  }


  cortes <- sort(cortes)

  cuantiles <- c()

  for(i in 1:length(cortes)){

    posicion <- min(which(tabla$Ni >= cortes[i]* N))

    if(cortes[i]* N == tabla$Ni[posicion]){

      cuantil <- mean(c(tabla$x[posicion],tabla$x[posicion+1]),na.rm=TRUE)

    } else {

      cuantil <- tabla$x[posicion]

    }

    cuantiles <- rbind(cuantiles,cuantil)


  }
  cuantiles <- as.data.frame(cuantiles)
  row.names(cuantiles) <- paste(cortes*100,"%",sep="")

  return(cuantiles)

}
