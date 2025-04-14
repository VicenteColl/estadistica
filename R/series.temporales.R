#' @title Series temporales.
#'
#' @description Esta función utiliza el método de las medias móviles (centradas) para extraer la tendencia de una serie temporal.
#' A partir de las medias móviles, también se obtienen los índices de variación estacional (IVE).
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrseriestemporales.png}{options: width="25\%" alt="Figure: qrmuestra1.png"}}
#' \if{latex}{\figure{qrseriestemporales.png}{options: width=3cm}}
#'
#' @usage series.temporales(x,
#'        variable = NULL,
#'        inicio_anual = 1,
#'        periodo_inicio = 1,
#'        frecuencia = 4,
#'        orden = frecuencia,
#'        prediccion_tendencia = FALSE,
#'        grafico = FALSE,
#'        exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param inicio_anual Año de inicio de la serie. Por defecto \code{inicio_anual = 1}.
#' @param periodo_inicio Periodo de inicio de la serie. Por defecto \code{perido_inicio = 1}, es decir, el primer periodo del año 1.
#' @param frecuencia Periodificación de la serie. Por defecto \code{frecuencia = 4}.
#' Si anual, frecuencia = 1.
#' Si semestral, frecuencia = 2.
#' Si cuatrimestral, frecuencia = 3.
#' Si trimestral, frecuencia = 4.
#' Si bimestral, frecuencia = 6.
#' Si mensual, frecuencia = 12.
#' Si semanal, frecuencia = 52.
#' Si diario, frecuencia = 360.
#' @param orden Orden (o puntos) de cálculo de la media móvil. Por defecto \code{orden = frecuencia}.
#' @param prediccion_tendencia vector de periodo temporal (\code{t=0}, origen de la serie) para el que se quiere obtener una predicción de la tendencia de la serie objeto de estudio.
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica la serie original, las medias móviles y la estimación por regresión de la tendencia, cambiar a \code{grafico = TRUE}.
#' @param exportar Para exportar los principales resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Esta función devuelve un objeto de la clase \code{list}.
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
#' @seealso \code{\link{regresion.simple}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#'@examples
#'
#' ejemplo_serie <- series.temporales(turistas2,
#' variable=2,
#' inicio_anual=2000,
#' periodo_inicio = 1)
#'
#' @import dplyr tidyr
#' @importFrom stats lm
#'
#' @export
series.temporales <- function(x,
                              variable = NULL,
                              inicio_anual = 1,
                              periodo_inicio = 1,
                              frecuencia = 4,
                              orden = frecuencia,
                              prediccion_tendencia = FALSE,
                              grafico = FALSE,
                              exportar = FALSE){

  old <- options()
  #on.exit(options(old))

  options(scipen = 999)

  varnames <- as.character(names(x))
  x <- data.frame(x)
  names(x) <- varnames

  if(is.null(variable)){
    if(length(x) == 1){
      x <- x
    } else{
      stop("El conjunto de datos seleccionado tiene mas de 1 variable.")
    }
  } else{
    if(length(variable) == 1){
      if(is.numeric(variable)){
        if(variable <= length(x)){
          variable <- variable
        } else{
          stop("Selecci\u00f3n err\u00f3nea de variable")
          }
      }
    }

    if(is.character(variable)){
      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
        }
    }
  }

    x <- x[,variable] %>% as.data.frame()
    names(x) <- varnames[variable]

  if(frecuencia == 4){
    time2 = "T"
  } else if(frecuencia==3){
    time2 = "C"
  } else if(frecuencia == 12){
    time2 = "M"
  } else if(frecuencia == 360){
    time2 = "D"
  } else if(frecuencia == 2){
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
  rename(variable_serie = varnames[variable]) %>%
  mutate(mediamovil = forecast::ma(variable_serie, order = orden, centre = TRUE)) %>%
  select(4,2,3,1,6)

# incluyo la variable t para el ajuste de tendencia
NonNAindex <- which(!is.na(mediasMoviles[5]))
firstNonNA <- min(NonNAindex)
lastNonNA <- max(NonNAindex)
mediasMoviles$t <- c(rep(NA,firstNonNA-1),0:(lastNonNA-firstNonNA),rep(NA,n-lastNonNA))

if(frecuencia != 1){
  ive <- mediasMoviles %>%
    mutate(paso1_ive = variable_serie/mediamovil) %>%
    select(time,periodo,paso1_ive) %>%
    arrange(periodo) %>%
    pivot_wider(names_from=periodo,values_from=paso1_ive) %>%
    summarize_at(vars(-1),mean,na.rm=TRUE)

  ivecorregido <- (ive/sum(ive[1,]))*frecuencia
  ivecorregido <- ivecorregido %>%
    t() %>%
    as.data.frame()
  names(ivecorregido) <- "IVE"
  rownames(ivecorregido) <- paste("IVE_",1:frecuencia,sep="")
  #sum(ivecorregido[1,])
}else{
  ivecorregido <- NULL
}


serie_regresion <- subset(mediasMoviles,!is.na(mediamovil))
serie_regresion <- serie_regresion %>%
  select(1,6,4,5)


#cambio nombre de objeto
names(mediasMoviles) <- c("Fecha","Periodo","Time",varnames[variable],
                          "Media.Movil","t")


media_t <- media(serie_regresion[2])
media_mediamovil <- media(serie_regresion[4])
varianza_t <- varianza(serie_regresion[2])
varianza_mediamovil <- varianza(serie_regresion[4])
covarianza_t_mediamovil <- as.numeric(covarianza(serie_regresion[,c(2,4)]))

modelo_series <- lm(mediamovil ~ t, data = serie_regresion)

constante_regresion <- as.numeric(modelo_series$coefficients[1])
coeficiente_regresion <- as.numeric(modelo_series$coefficients[2])
correlacion_t_mediamovil <- as.numeric(correlacion(serie_regresion[,c(2,4)]))
coeficiente_determinacion <- summary(modelo_series)$r.squared
varianza_explicada <- varianza(modelo_series$fitted.values)
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


if(isTRUE(grafico)){
  if(frecuencia!=1){
    p <-  ggplot(serie_regresion) +
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
      labs(title=paste("Serie temporal de ",varnames[variable],sep=""),
           subtitle=paste("Ajuste lineal: ",varnames[variable],"=",round(modelo_series$coefficients[1],5),if_else(modelo_series$coefficients[2] >=0, "+", ""),round(modelo_series$coefficients[2],5)," * t",sep=""),
           x="Periodo",
           y=varnames[variable]) +
  theme(axis.text.x=element_text(size=6,angle=90,vjust=0.2))

  } else{
    p <-  ggplot(serie_regresion) +
      geom_point(aes(x = t, y = variable_serie)) +
      geom_line(aes(x = t, y = variable_serie)) +
      geom_smooth(aes(t,mediamovil),
                  method = "lm",
                  formula= 'y ~ x',
                  se = FALSE,
                  color = "blue") +
      scale_x_continuous(breaks = serie_regresion$t, labels = serie_regresion$fecha) +
      labs(title=paste("Serie temporal de ",varnames[variable],sep=""),
           subtitle=paste("Ajuste lineal: ",varnames[variable],"=",round(modelo_series$coefficients[1],5),if_else(modelo_series$coefficients[2] >=0, "+", ""),round(modelo_series$coefficients[2],5)," * t",sep=""),
           x="Periodo",
           y=varnames[variable]) +
      theme(axis.text.x=element_text(size=6,angle=90,vjust=0.2))

  }
}else{
  p <- NULL
}


if(prediccion_tendencia){
  pronostico <- constante_regresion + coeficiente_regresion *prediccion_tendencia
  pronosticos <- data.frame(valor_t = prediccion_tendencia,pronosticos = pronostico)
} else{
  pronosticos <- NULL
}


# if (exportar) {
#   filename <- paste("Resultado series temporales (",Sys.time(), ").xlsx", sep = "")
#   filename <- gsub(" ", "_", filename)
#   filename <- gsub(":", ".", filename)
#
#   if(frecuencia!=1){
#
#     if(prediccion_tendencia){
#       lista <- list(mediasMoviles,ivecorregido,serie_regresion,resultados_regresion,pronosticos)
#
#       rio::export(lista, rowNames = TRUE, filename, sheetName=c("Medias moviles",
#                                                               "IVE",
#                                                               "Datos ajuste tendencia",
#                                                               "Modelo ajuste",
#                                                               "Pronosticos"))
#     }else{
#       lista <- list(mediasMoviles,ivecorregido,serie_regresion,resultados_regresion)
#
#       rio::export(lista, rowNames = TRUE, filename, sheetName=c("Medias moviles",
#                                                               "IVE",
#                                                               "Datos ajuste tendencia",
#                                                               "Modelo ajuste"))
#     }
#   }else{
#     if(prediccion_tendencia){
#       lista <- list(mediasMoviles,serie_regresion,resultados_regresion,pronosticos)
#
#       rio::export(lista, rowNames = TRUE, filename, sheetName=c("Medias moviles",
#                                                               "Datos ajuste tendencia",
#                                                               "Modelo ajuste",
#                                                               "Pronosticos"))
#     }else{
#       lista <- list(mediasMoviles,serie_regresion,resultados_regresion)
#
#       rio::export(lista, rowNames = TRUE, filename, sheetName=c("Medias moviles",
#                                                               "Datos ajueste tendencia",
#                                                               "Modelo ajuste"))
#     }
#   }
#
# }

if (exportar) {
  # Prepara el nombre del archivo (formato más limpio)
  filename <- paste("Resultado_series_temporales_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx", sep = "")

  # Crear el workbook
  wb <- openxlsx::createWorkbook()

  # exportacion segun parametros
  if (frecuencia != 1) {
    if (prediccion_tendencia) {
      .add_sheet_with_style(wb, "Medias_moviles", mediasMoviles)
      .add_sheet_with_style(wb, "IVE", ivecorregido)
      .add_sheet_with_style(wb, "Datos_ajuste_tendencia", serie_regresion)
      .add_sheet_with_style(wb, "Modelo_ajuste", resultados_regresion)
      .add_sheet_with_style(wb, "Pronosticos", pronosticos)
    } else {
      .add_sheet_with_style(wb, "Medias_moviles", mediasMoviles)
      .add_sheet_with_style(wb, "IVE", ivecorregido)
      .add_sheet_with_style(wb, "Datos_ajuste_tendencia", serie_regresion)
      .add_sheet_with_style(wb, "Modelo_ajuste", resultados_regresion)
    }
  } else {
    if (prediccion_tendencia) {
      .add_sheet_with_style(wb, "Medias_moviles", mediasMoviles)
      .add_sheet_with_style(wb, "Datos_ajuste_tendencia", serie_regresion)
      .add_sheet_with_style(wb, "Modelo_ajuste", resultados_regresion)
      .add_sheet_with_style(wb, "Pronosticos", pronosticos)
    } else {
      .add_sheet_with_style(wb, "Medias_moviles", mediasMoviles)
      .add_sheet_with_style(wb, "Datos_ajuste_tendencia", serie_regresion)
      .add_sheet_with_style(wb, "Modelo_ajuste", resultados_regresion)
    }
  }

  # Guardar el archivo
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

return(list('Medias_moviles' = mediasMoviles,
            'IVE' = ivecorregido,
            'Datos_ajuste_tendencia'= serie_regresion,
            'Modelo_ajuste' = resultados_regresion,
            'Pronosticos' = pronosticos,
            'Grafico' = p))

on.exit(options(old))

}
