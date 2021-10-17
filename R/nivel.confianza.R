#' @encoding UTF-8
#' @title Nivel de confianza.
#'
#' @description Esta función simula una población de tamaño 100,000 de la que se extraen diversas muestras y construye los correspondientes intervalos de confianzas. El objetivo es transmitir el concepto de nivel de confianza.
#' @usage nivel.confianza(min.pob,
#' max.pob,
#' muestras = 200,
#' n = 100,
#' confianza = 0.95,
#' grafico = TRUE,
#' exportar = FALSE,
#' replicar = TRUE)
#'
#' @param min.pob Es un valor numérico que indica el valor mínimo poblacional. Por defecto min.pob = 2000
#' @param max.pob Es un valor numérico que indica el valor máximo poblacional. Por defecto max.pob = 45000
#' @param muestras Es un valor numérico entre 50 y 10000 que indica el número de muestras que se extraen sin reemplazamiento de la población. Por defecto muestras = 200
#' @param n Es un valor numérico entre 25 y 2000 que indica el tamaño de la muestra. Por defecto n = 100.sumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, confianza = 0.95 (95 por ciento)
#' @param grafico Si grafico = TRUE se representan los intervalos de confianza de las muestras seleccionadas y la media poblacional.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
#' @param replicar Es un valor lógico. Si replicar=TRUE se fija una semilla para que los resultados sean reproducibles. Si replicar=FALSE los resultados serán aleatorios y cambiarán en cada realización.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Universidad de Valencia (España)
#'
#' @seealso \code{\link{ic.media}}
#'
#' @references
#' Casas José M. () Inferencia estadística. Editoral: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' @import dplyr ggplot2
#'
#' @export
nivel.confianza <- function(min.pob = 2000,
                            max.pob = 45000,
                            muestras = 200,
                            n = 100,
                            confianza = 0.95,
                            grafico = TRUE,
                            exportar = FALSE,
                            replicar = TRUE){

  if(replicar){

    set.seed(123456)

  }

# se define una población de 100000 observaciones que toma valores a y b

if( max.pob <= min.pob) {
  stop("Los valores introducidos no son correctos")
}

poblacion <- sample(min.pob:max.pob,100000,replace=TRUE)
media_poblacional <- mean(poblacion)

# datos muestrales

if(muestras < 50 | muestras > 10000){

  stop("Elige un n\u00famero de muestras entre 50 y 10000")
}

if(n < 25 | n > 2000){

  stop("Elige un tama\u00f1o para la muestra de entre 50 y 2000")
}

muestra_select <- c()
medias_muestra <- c()
varianzas_muestra <- c()

for(i in 1:muestras){
  seleccion <- sample(poblacion,n,replace=TRUE)
  media <- media(seleccion)
  varianza <- varianza(seleccion)

  muestra_select <- cbind(muestra_select,seleccion)
  medias_muestra <- rbind(medias_muestra,media)
  varianzas_muestra <- rbind(varianzas_muestra, varianza)

}

resultados_muestrales <- as.data.frame(cbind(1:muestras,medias_muestra,varianzas_muestra))
colnames(resultados_muestrales) <- c("muestra","media.muestral","varianza.muestral")
row.names(resultados_muestrales) <- NULL

# fijamos el nivel de confianza para calcular el
# intervalo confianza media muestral con var pob desconocida

z <- qnorm((1-confianza)/2, lower.tail = FALSE)

resultados_muestrales <- resultados_muestrales %>% mutate(inferior = media.muestral - z * sqrt(varianza.muestral/n),
                                                          superior = media.muestral + z * sqrt(varianza.muestral/n),
                                                          incluido = case_when(media_poblacional >= inferior & media_poblacional <= superior ~ 1,
                                                                               TRUE ~ 0))
porcentaje.intervalos <- resultados_muestrales %>% summarize(porcentaje = sum(incluido)/n())


if(grafico){

  df <- resultados_muestrales %>%
    select(muestra,inferior,superior,incluido) %>%
    mutate(color = ifelse(incluido == 0, "red", "blue")) %>%
    pivot_longer(cols= c(2,3))

  if(muestras > 200){

    df <- resultados_muestrales[sample(muestras,200),] %>%
      arrange(muestra) %>%
      select(muestra,inferior,superior,incluido) %>%
      mutate(color = ifelse(incluido == 0, "red", "blue")) %>%
      pivot_longer(cols= c(2,3))

  } else{

    df <- resultados_muestrales %>%
      select(muestra,inferior,superior,incluido) %>%
      mutate(color = ifelse(incluido == 0, "red", "blue")) %>%
      pivot_longer(cols= c(2,3))
  }

  interactivo <- as.numeric(readline('\u00bfQuieres hacer el gr\u00e1fico interactivo?: \n 1. "S\u00ed." \n 2. "No." \n'))

    plot <-  ggplot(df, aes(x=value, y=muestra)) +
      geom_line(aes(group = muestra), color = "grey") +
      geom_point(size=1, color = df$color) +
      geom_vline(aes(xintercept = media_poblacional), color = "darkred",size=1) +
      geom_text(aes(x=media_poblacional, label ="media poblaci\u00f3n", y = muestras),
                color = "darkred", hjust = -0.05, vjust=-0.5, size = 3) +
      geom_text(aes(x=media_poblacional, label=round(media_poblacional,2), y=0), hjust= -0.10, vjust = 1.2, size = 3, color="darkred") +
      scale_color_identity() +
      labs(title = paste("Intervalos de confianza de un total de ",muestras," muestras",sep=""),
           y="N\u00famero de muestra",
           x=paste("El ",round(porcentaje.intervalos *100,2),"% de los intervalos de confianza\ncalculados contienen el valor de la media poblacional",sep=""))

    if(interactivo == 1){

      plot <- plotly::ggplotly(plot, tooltip=c("y","x"))

    }

}

  if (exportar) {
    filename <- paste("Intervalos de confianza"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(resultados_muestrales, row.names = TRUE, file = filename)
  }

  if(grafico){

    return(list(resultados_muestrales, porcentaje.intervalos,plot))

  } else{

    return(list(resultados_muestrales, porcentaje.intervalos))

  }

}
