
install.packages("estadistica")
library(estadistica)


contraste.homogeneidad <- function(x,
                                 introducir = F,
                                 alfa = 0.05,
                                 grafico = FALSE){
  
  
  if(isFALSE(introducir)) {
    
    if(ncol(x) != 2) {
      stop("La base de datos introducida debe contener solo 2 columnas")
    }
    
    data_frame_obs <- data.frame(x)
    matriz_obs <- table(data_frame_obs[,1], data_frame_obs[,2]); print(matriz_obs)
    
    respuesta <- readline(prompt ='Es esta la matriz de datos observados? \n 1. "Si" \n 2. "No" \n')
    
    if (respuesta == "2"){
      print("Has marcado que esta no es esta la matriz de datos observados, introduce o modifica lo que creas necesario")
      matriz_obs <- edit(matriz_obs); matriz_obs  
      
    } else if (respuesta == "1"){
      print("¡Perfecto! Seguimos con estos datos")
      
    } else {
      stop("El comando introducido no es correcto")
      
    } # End if respuesta
    
    if( sum(matriz_obs < 5) > 0){
      
      stop("Alguna de las categorías es menor que 5. Debes reagrupar categorias para cumplir las hipótesis del test y volver a ejecutar. Se recomiendo agrupar la categória que contiene menos de 5 elementos")
      
    }  

  } else {
    
    nfilas <- as.numeric(readline(prompt = "Intoduce el número de categorias de la primera variable: ")) 
    ncolumnas <- as.numeric(readline(prompt = "Intoduce el número de categorias de la segunda variable: "))
    
    nombre_filas <- nombre_columnas <- c()
    for (j in 1:nfilas){
      
      nombre_filas <- c(nombre_filas, readline(prompt = paste("Introduce el nombre de la fila número ", j,": ", sep = "")))
      
    }
    
    for (k in 1:ncolumnas){
      
      nombre_columnas <- c(nombre_columnas, readline(prompt = paste("Introduce el nombre de la columna número ", k,": ", sep = "")))
      
    }
    
    x <- matrix(0, ncol = ncolumnas, nrow = nfilas)
    rownames(x) <- nombre_filas
    colnames(x) <- nombre_columnas
    
    matriz_obs <- edit(x);matriz_obs
    
    if( sum(matriz_obs < 5) > 0){
      
      stop("Alguna de las categorías es menor que 5. Debes reagrupar categorias para cumplir las hipótesis del test y volver a ejecutar. Se recomiendo agrupar la categória que contiene menos de 5 elementos")
      
    }  
    
  } # End if introducir
    
      n <- sum(matriz_obs)
      mar_x <- apply(matriz_obs, 1, sum)
      mar_y <- apply(matriz_obs, 2, sum)
      matriz_esp <- crossprod(t(mar_x), mar_y) / n
    
      g.l <- (nrow(matriz_obs) - 1) * (ncol(matriz_obs) - 1)      

      if (g.l > 1){
        estadistico.prueba <- sum(((matriz_obs - matriz_esp) ^ 2)  / matriz_esp)
      } else {
        estadistico.prueba <- sum(((abs(matriz_obs - matriz_esp) -0.5) ^ 2)  / matriz_esp)
        warning('Se ha aplicado la correción de Yates')
      } # End if
  

  
  valor_critico <- qchisq(alfa, g.l, lower.tail = F)
  
  if(estadistico.prueba < valor_critico ){
    
    print(paste("No se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))
    
  } else{
    
    print(paste("Se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra fuera de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))
    
  }
  
  pvalor <- pchisq(estadistico.prueba, g.l, lower.tail = F)
  
  H0 <- paste("Las variables son homogéneas")
  CH <- cbind(H0, estadistico.prueba, round(pvalor, 4))
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL
  
  if(isTRUE(grafico)){
    
    percentil99 <- qchisq(.9999, g.l)

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
    
  } #End if grafico
  
  
} # End of function 
  
  
  
  