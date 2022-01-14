#' @title Series temporales.
#'
#' @description Esta función utiliza el método de las medias móviles (centradas) para suavizar la componente irregular de una serie temporal.
#' A partir de las medias móviles, también se obtienen los índices de variación irregular (IVE).
#'
#' series.temporales(x,
#' variable = NULL,
#' inicio_anual = 1,
#' periodo_inicio = 1,
#' frecuencia = 4,
#' orden = frecuencia)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param inicio_anual Año de inicio de la serie. Por defecto \code{inicio_anual = 1}.
#' @param periodo_inicio Periodo de inicio de la serie. Por defecto \code{perido_inicio = 1}, es decir, el primer periodo del año 1.
#' @param frecuencia Periodificación de la serie. Por defecto \code{frecuencia = 4}.
#' Si anual, frecuencia = 1
#' Si semestral, frecuencia = 2
#' Si cuatrimestral, frecuencia = 3
#' Si trimestral, frecuencia = 4
#' Si bimestral, frecuencia = 6
#' Si mensual, frecuencia = 12
#' Si semanal, frecuencia = 52
#' Si diario, frecuencia = 360
#' @param orden Orden (o puntos) de cálculo de la media móvil. Por defecto \code{orden = frecuencia}.
#' @param prediccion Orden de la media móvil. Por defecto orden = frecuencia.
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica la serie original, las medias móviles y la estimación por regresión de la tendencia, cambiar a \code{grafico = TRUE}.
#' @param exportar Para exportar los principales resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Esta función devuelve la covarianza en un objeto de la clase \code{data.frame}.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'

#' @note
#'
#' @seealso \code{\link{varianza}}, \code{\link{desviacion}},\code{\link{matriz.covar}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom forecast ma
#' @import dplyr forecast zoo
#'
#' @export
series.temporales <- function(x,
                              variable = NULL,
                              inicio_anual = 1,
                              periodo_inicio = 1,
                              frecuencia = NULL,
                              orden = frecuencia,
                              prediccion = FALSE,
                              grafico = FALSE,
                              exportar = FALSE){

  old <- options()
  on.exit(options(old))

  options(scipen = 999)

  x <- as.data.frame(x)

  varnames <- colnames(x)

  if(length(x) > 1 ) {

    variable <- readline(prompt = "Intoduce el nombre de la variable: ")

  } else{

    variable <- varnames

  }

  if(is.character(variable)){
    if(variable %in% varnames){
      variable = which(varnames == variable)
    } else {
      stop("El nombre de la variable no es valido")
    }
  }

  x <- as.data.frame(x) %>%
    dplyr::select(all_of(variable))

  y <- varnames[variable] # nombre de la variable seleccionada

  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer")) {
    stop("No puede construirse la tabla de frecuencias, la variable que has\n
         seleccionado es car\u00e1cter")
  }

  if(length(x) > 1){
    stop("Esta funci\u00f3n solo puede contruir la tabla de frecuencias de una variable")
    print("Para obtener la tabla de frecuencias de mas de una variable utiliza la funci\u00f3n apply")
  }


if(frecuencia == 4){
  time2 = "T"
} else if(frecuencia==3){
  time2 = "C"
} else if(frecuencia == 12){
  time2 = "M"
} else if(frecuencia == 360){
  time2 = "D"
} else if(frecuencia == 2){
  orden = 2
  time2 = "S"
} else if(frecuencia == 52){
  time2 = "s"
} else if(frecuencia == 6){
  time2 = "B" # bimestral (cada dos meses)
} else{
  time2 = NULL
}


orden = orden

n <- nrow(x)

period <- rep(1:frecuencia,ceiling((n+periodo_inicio-1)/frecuencia))
time = rep(1:ceiling((n+periodo_inicio-1)/frecuencia),each=frecuencia)

if(periodo_inicio != 1){
  x$periodo <- period[-c(1:(periodo_inicio-1))][1:n]

  if(inicio_anual==1){
    x$time <- time[-c(1:(periodo_inicio-1))][1:n]
  } else{
    x$time <- inicio_anual + time[-c(1:(periodo_inicio-1))][1:n] -1
  }
}else{

  x$periodo <- period[1:n]

  if(is.null(inicio_anual)){
    x$time <- time[1:n]
  } else{
    x$time <- inicio_anual + time[1:n] -1
  }

}

if(frecuencia !=1){
  x$fecha <- paste(x$periodo,time2,"_",x$time,sep="")
}else{
  x$fecha <- x$time
}

x$id <- 1:n


mediasMoviles <- x %>%
  rename(variable_serie = varnames) %>%
  mutate(mediamovil = ma(variable_serie, order = orden, centre = TRUE)) %>%
  select(4,2,3,1,6)

if(frecuencia != 1){
  ive <- mediasMoviles %>%
    mutate(paso1_ive = variable_serie/mediamovil) %>%
    select(time,periodo,paso1_ive) %>%
    arrange(periodo) %>%
    pivot_wider(names_from=periodo,values_from=paso1_ive) %>%
    summarize_at(vars(-1),mean,na.rm=TRUE)

  ivecorregido <- (ive/sum(ive[1,]))*frecuencia
  rownames(ivecorregido) <- "IVE"
  #sum(ivecorregido[1,])
}else{
  ivecorregio <- NULL
}

serie_regresion <- subset(mediasMoviles,!is.na(mediamovil))
serie_regresion <- serie_regresion %>%
  mutate(t=0:(nrow(serie_regresion)-1)) %>%
  select(1,6,4,5)

if(grafico){
  if(frecuencia!=1){
    plot <-  ggplot(serie_regresion) +
      geom_point(aes(x = t, y = variable_serie)) +
      geom_line(aes(x = t, y = variable_serie)) +
      geom_point(aes(x = t, y = mediamovil),size=1,color="red") +
      geom_line(aes(x = t, y = mediamovil),size=1,color="red") +
      geom_smooth(aes(t,mediamovil),
                  method = "lm",
                  formula= 'y ~ x',
                  se = FALSE,
                  color = "blue") +
      scale_x_continuous(breaks = serie_regresion$t, labels = serie_regresion$fecha) +
      labs(x="Periodo",y=varnames) +
      theme(axis.text.x=element_text(size=6,angle=90,vjust=0.2))

  } else{
    plot <-  ggplot(serie_regresion) +
      geom_point(aes(x = t, y = variable_serie)) +
      geom_line(aes(x = t, y = variable_serie)) +
      geom_smooth(aes(t,mediamovil),
                  method = "lm",
                  formula= 'y ~ x',
                  se = FALSE,
                  color = "blue") +
      scale_x_continuous(breaks = serie_regresion$t, labels = serie_regresion$fecha) +
      labs(x="Periodo",y=varnames) +
      theme(axis.text.x=element_text(size=6,angle=90,vjust=0.2))
  }
}else{
  plot <- NULL
}

media_t <- as.numeric(media(serie_regresion[2]))
media_mediamovil <- as.numeric(media(serie_regresion[4]))
varianza_t <- as.numeric(varianza(serie_regresion[2]))
varianza_mediamovil <- as.numeric(varianza(serie_regresion[4]))
covarianza_t_mediamovil <- as.numeric(covarianza(serie_regresion[,c(2,4)]))

modelo_series <- lm(mediamovil ~ t, data = serie_regresion)

constante_regresion <- as.numeric(modelo_series$coefficients[1])
coeficiente_regresion <- as.numeric(modelo_series$coefficients[2])
correlacion_t_mediamovil <- as.numeric(correlacion(serie_regresion[,c(2,4)]))
coeficiente_determinacion <- summary(modelo_series)$r.squared
varianza_explicada <- as.numeric(varianza(modelo_series$fitted.values))
varianza_residual <- varianza_mediamovil - varianza_explicada

resultados_regresion <- data.frame(c(media_t,
                                     media_mediamovil,
                                     varianza_t,
                                     varianza_mediamovil,
                                     covarianza_t_mediamovil,
                                     correlacion_t_mediamovil,
                                     constante_regresion,
                                     coeficiente_regresion,
                                     coeficiente_determinacion,
                                     varianza_explicada,
                                     varianza_residual))
names(resultados_regresion) <- "Resultados"

rownames(resultados_regresion) <- c("media t","media mediamovil","varianza t",
                                 "varianza mediamovil","covarianza t_mediamovil",
                                 "correlacion t_mediamovil","constante regresion",
                                 "coeficiente regresion","coeficiente determinacion",
                                 "varianza explicada","varianza residual")


if(prediccion){
  pronostico <- constante_regresion + coeficiente_regresion *prediccion
  pronosticos <- data.frame(valor_t = prediccion,pronosticos = pronostico)
} else{
  pronosticos <- NULL
}


if (exportar) {
  filename <- paste("Resultado series temporales (",Sys.time(), ").xlsx", sep = "")
  filename <- gsub(" ", "_", filename)
  filename <- gsub(":", ".", filename)

  if(frecuencia!=1){

    if(prediccion){
      lista <- list(mediasMoviles,ivecorregido,resultados_regresion,pronosticos)

      rio::export(lista, row.names = T, filename, sheetName=c("Medias moviles",
                                                              "IVE",
                                                              "Modelo ajuste",
                                                              "Pronosticos"))
    }else{
      lista <- list(mediasMoviles,ivecorregido,resultados_regresion)

      rio::export(lista, row.names = T, filename, sheetName=c("Medias moviles",
                                                              "IVE",
                                                              "Modelo ajuste"))
    }
  }else{
    if(prediccion){
      lista <- list(mediasMoviles,resultados_regresion,pronosticos)

      rio::export(lista, row.names = T, filename, sheetName=c("Medias moviles",
                                                              "Modelo ajuste",
                                                              "Pronosticos"))
    }else{
      lista <- list(mediasMoviles,resultados_regresion)

      rio::export(lista, row.names = T, filename, sheetName=c("Medias moviles",
                                                              "Modelo ajuste"))
    }
  }

}

return(list('Medias_moviles' = mediasMoviles,
            'IVE' = ivecorregido,
            'Modelo_ajuste' = resultados_regresion,
            'Pronosticos' = pronosticos,
            'Grafico' = plot))

}
