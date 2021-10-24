mediana.int <- function(x, pesos = NULL){

  x <- as.data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la median, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    x <- tidyr::drop_na(x)
    y <- names(x)
    names(x) <- "x"

    N <- nrow(x)
    tabla <- x %>% group_by(x) %>%
      count() %>%
      ungroup() %>%
      mutate(Ni = cumsum(n))

  } else{

    N <- sum(pesos)

    tabla <- data.frame(x,pesos) %>%
      na.omit
    y <- names(tabla)
    names(tabla) <- c("x","pesos")

    tabla <- tabla %>%
      arrange(x) %>%
      mutate(Ni = cumsum(pesos))

  }

  posicion <- min(which(tabla$Ni >= N/2))

  if(N/2 == tabla$Ni[posicion]){

    mediana <- mean(c(tabla$x[posicion],tabla$x[posicion+1]),na.rm=TRUE)

  } else {

    mediana <- tabla$x[posicion]

  }

  return(mediana)
}
