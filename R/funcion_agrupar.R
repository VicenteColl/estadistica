
agrupar <- function(observadas,esperadas){
  
  observadas <- matriz$Freq_obs
  esperadas <- matriz$Freq_esp
  
  df <- data.frame(matrix(ncol = 2, nrow = length(observadas)))
  names(df) <- c("o","t")
  
  df$t <- esperadas
  df$o <- observadas
  
  df2t <- c()
  df2o <- c()
  intermediot <- c()
  intermedioo <- c()
  intermedio2t <- NULL
  intermedio2o <- NULL
  df21t <- 0
  df21o <- 0
  df23t <- 0
  df23o <- 0
  
  
  if(df$t[1]<5){
    i=1
    while(i <= nrow(df)){
      
      nt = df$t[i]
      
      if(nt >= 5){
        
        df2t <- df$t[i]
        df2o <- df$o[i]
        
      } else{
        
        j=1
        
        while((nt < 5)  & ((i+j) <= nrow(df))){ #pq exijo que sea mayor a 5
          
          
          if(sum(df$t[i:(i+j)]) >= 5){ # exijo que sea mayor a 5
            df21t <- sum(df$t[i:(i+j)])
            df21o <- sum(df$o[i:(i+j)])
            
          } 
          
          
          j = j + 1    
          
          if((i+j) > nrow(df)){
            
            df22t <- intermediot[length(intermediot)] + df22t
            intermedio2t <- df22t
            df22o <- intermedioo[length(intermedioo)] + df22o
            intermedio2o <- df22o
            
            break
          }
          
          nt <- sum(df$t[i:(i+j)])
          df22t <- sum(df$t[i:(i+j)])
          df22o <- sum(df$o[i:(i+j)])
          
        }
        
        
        if((df21t >= 5) & (df21t < df22t)){
          df2t <- df21t
          df2o <- df21o
          i <- i + j 
        }else{
          df2t <- df22t
          df2o <- df22o
          i <- i + j + 1
          
        }
        
        j = 0
        
      }
      

      if(length(intermedio2t)!=0){
        
        intermediot[length(intermediot)] <- intermedio2t
        intermedioo[length(intermedioo)] <- intermedio2o
        
      }else{
        
        intermediot <- c(intermediot,df2t)
        intermedioo <- c(intermedioo,df2o)
        
      }
      
      intermedio2t <- c()
      intermedio2o <- c()
      
      
      ult.intermedio <- length(intermediot)
      
      
      # Comprobar si es el penúltimo elemento
      
      if(i == (nrow(df)-1)){
        
        if(df$t[i] < 5){
          
          df23t <- sum(df$t[i:(i+1)])
          df23o <- sum(df$o[i:(i+1)])
          
          i = i + 1
          
        } else{
          
          df2t <- df$t[i]
          intermediot <- c(intermediot,df2t)
          
          
          df2o <- df$o[i]
          intermedioo <- c(intermedioo,df2o)
          
        }
        
      }
      
      # comprobar si es el último elemento del df original
      if(i == nrow(df)){
        
        if(df23t !=0 & df23t < 5){
          
          intermediot[ult.intermedio] <- intermediot[ult.intermedio] + df23t
          intermedioo[ult.intermedio] <- intermedioo[ult.intermedio] + df23o
          
        } else{
          
          if(df$t[i]<5){
            
            intermediot[ult.intermedio] <- intermediot[ult.intermedio] + df$t[i]
            intermedioo[ult.intermedio] <- intermedioo[ult.intermedio] + df$o[i]
            
          } else{
            
            intermediot <- c(intermediot,df$t[i])
            intermedioo <- c(intermedioo,df$o[i]) 
            
          }
          
        }
        
        i = i + 1
      }
      

      if(i > nrow(df)){
        break
      }
      
    }
    
  }else{
    for(i in 1:nrow(df)){
      
      nt = df$t[i]
      
      if(nt >= 5){
        
        df2t <- df$t[i]
        df2o <- df$o[i]
        
      } else{
        
        j=1
        
        while((nt < 5)  & ((i+j) <= nrow(df))){ #pq exijo que sea mayor a 5
          
          
          if(sum(df$t[i:(i+j)]) >= 5){ # exijo que sea mayor a 5
            df21t <- sum(df$t[i:(i+j)])
            df21o <- sum(df$o[i:(i+j)])
            
          } 
          
          
          j = j + 1    
          
          if((i+j) > nrow(df)){
            
            df22t <- intermediot[length(intermediot)] + df22t
            intermedio2t <- df22t
            df22o <- intermedioo[length(intermedioo)] + df22o
            intermedio2o <- df22o
            
            break
          }
          
          nt <- sum(df$t[i:(i+j)])
          df22t <- sum(df$t[i:(i+j)])
          df22o <- sum(df$o[i:(i+j)])
          
        }
        
        
        if((df21t >= 5) & (df21t < df22t)){
          df2t <- df21t
          df2o <- df21o
          i <- i + j 
        }else{
          df2t <- df22t
          df2o <- df22o
          i <- i + j + 1
          
        }
        
        j = 0
        
      }
      

      if(length(intermedio2t)!=0){
        
        intermediot[length(intermediot)] <- intermedio2t
        intermedioo[length(intermedioo)] <- intermedio2o
        
        
      }else{
        
        intermediot <- c(intermediot,df2t)
        intermedioo <- c(intermedioo,df2o)
        
        
      }
      
      intermedio2t <- c()
      intermedio2o <- c()
      
      
      ult.intermedio <- length(intermediot)

      
      # Comprobar si es el penúltimo elemento
      
      if(i == (nrow(df)-1)){
        
        if(df$t[i] < 5){
          
          df23t <- sum(df$t[i:(i+1)])
          df23o <- sum(df$o[i:(i+1)])
          
          i = i + 1
          
        } else{
          
          df2t <- df$t[i]
          intermediot <- c(intermediot,df2t)
          
          
          df2o <- df$o[i]
          intermedioo <- c(intermedioo,df2o)
          
        }
        
      }

      # comprobar si es el último elemento del df original
      if(i == nrow(df)){
        
        if(df23t !=0 & df23t < 5){
          
          intermediot[ult.intermedio] <- intermediot[ult.intermedio] + df23t
          intermedioo[ult.intermedio] <- intermedioo[ult.intermedio] + df23o
          
        } else{
          
          if(df$t[i]<5){
            
            intermediot[ult.intermedio] <- intermediot[ult.intermedio] + df$t[i]
            intermedioo[ult.intermedio] <- intermedioo[ult.intermedio] + df$o[i]
            
          } else{
            
            intermediot <- c(intermediot,df$t[i])
            intermedioo <- c(intermedioo,df$o[i]) 
            
          }
          
        }
        
        i = i + 1
      }
      
      if(i > nrow(df)){
        break
      }
      
    }
    
  }
  
  
  
  
  frecuencias <- data.frame(Freq_obs= intermedioo ,
                            Freq_esp = intermediot )
  
  return(frecuencias)
  
}

