ric <- function(x, variable = NULL, pesos = NULL, grafico = FALSE){

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse el recorrido intercuart\u00edlico, alguna variable que has seleccionado no es cuantitativa")
  }
  cortes = c(0.25,0.75)



  ric <- apply(x,2,cuantiles_int)
  ric <- as.data.frame(ric)
  ric[3,] <- ric[2,]-ric[1,]

  names(ric) <- names(x)
  row.names(ric) <- c(paste(cortes*100,"%",sep=""),"RIC")

  if(isTRUE(grafico)){
    x <-  x  %>%pivot_longer(cols = 1:2,values_to ="valor")
    ggplot(x,aes(name,valor)) +
      geom_boxplot()

  }


  return(ric)

}
