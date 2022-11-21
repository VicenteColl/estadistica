
contraste.bondad.ajuste <- function(x,
                                    variable = NULL,
                                    introducir = FALSE,
                                    distribucion = "equiprobable",
                                    alfa = 0.05,
                                    grafico = FALSE){

  if(isFALSE(introducir)) {

    if(is.numeric(x)){

      matriz <- data.frame("Valores" = unique(x),
                           "Freq_obs" = as.numeric(table(x)))

      print(matriz)

      respuesta <- readline(prompt ='Estas son las frecuencias observadas? \n 1. "Si" \n 2. "No" \n')

      if (respuesta == "2"){
        print("Has marcado que estas no son las frecuencias, introduce o modifica lo que creas necesario")
        matriz <- edit(matriz); matriz

      } else if (respuesta == "1"){
        print("?Perfecto! Seguimos con estos datos")

      } else {
        stop("El comando introducido no es correcto")

      } # End if respuesta

    } else {

      stop("x debe ser un vector de n?meros")

    } # End if is.numeric

  } else {

    nfilas <- as.numeric(readline(prompt = "Intoduce el n?mero de filas: "))

    x <- matrix(0, nrow = nfilas, ncol=2)


    matriz <- edit(x)
    matriz <- as.data.frame(matriz); matriz
    colnames(matriz) <- c("Valores", "Freq_obs")

  } # End if isFALSE


    if (distribucion == "equiprobable"){

      # Equiprobable

      n <- length(matriz$Freq_obs)
      matriz$Freq_esp <-  sum(matriz$Freq_obs) / length(matriz$Freq_obs)

    } else if (distribucion == "poisson"){

      # poisson
      n <- length(matriz$Freq_obs) - 1
      lambda <- as.numeric(crossprod(matriz$Valores, matriz$Freq_obs)) / sum(matriz$Freq_obs)
      matriz$Freq_esp <-dpois(matriz$Valores,lambda) * sum(matriz$Freq_obs)

    } else if (distribucion == "binomial"){

      # Binomial
      n <- length(matriz$Freq_obs) - 1
      media <- as.numeric(crossprod(matriz$Valores, matriz$Freq_obs)) / sum(matriz$Freq_obs)
      p <- media / n

      matriz$Freq_esp <- dbinom(matriz$Valores, n, p ) * sum(matriz$Freq_obs)


    } else {

      stop("No has introducido una distribuci?n de probabilidad disponible")


    } # End of distribucion

    estadistico.prueba <- sum((matriz$Freq_obs - matriz$Freq_esp)^2/ matriz$Freq_esp)
    g.l <- n - 1

    valor_critico <- qchisq(alfa, g.l, lower.tail = F)


    if(estadistico.prueba < valor_critico ){

      print(paste("No se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

    } else{

      print(paste("Se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra fuera de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

    }

    pvalor <- pchisq(estadistico.prueba, g.l, lower.tail = F)

    H0 <- paste("Los datos siguen una distribuci?n ", distribucion)
    CH <- cbind(H0, estadistico.prueba, round(pvalor, 4))
    CH <- as.data.frame(CH)
    names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
    row.names(CH) <- NULL

    if(isTRUE(grafico)){

      percentil99 <- qchisq(.9999, g.l)
      estadistico.prueba <- 6.4

      data <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
      data$y <-dchisq(data$x, g.l)

      name_plot <- ggplot(data, aes(x, y)) +
        geom_area(fill = "#C4961A") +
        geom_area(data = subset(data, x < valor_critico), fill = "#008B8B") +
        geom_vline(xintercept = 0, color = "black") +
        geom_vline(xintercept = estadistico.prueba, color = "#A52A2A", linetype = "dashed" , size = 1) +
        labs(title = paste("Distribuci\u00f3n Chi-Cuadrado con ", g.l," grados de libertad", sep=""), x = "", y = "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = round(c(0L, estadistico.prueba, valor_critico), 2)) +
        theme(axis.text.x = element_text(angle = 45))

      return(list(CH, name_plot))

    } else{

      return(CH)

    }

} # End of function

