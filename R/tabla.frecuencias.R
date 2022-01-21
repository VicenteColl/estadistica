#' @title Tabla de frecuencias.
#'
#' @description Esta función presenta la distribución de frecuencias de una variable.
#'
#' \if{html}{\figure{qrtablafrecuencias.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrtablafrecuencias.png}{options: width=3cm}}
#'
#' @usage tabla.frecuencias(x,
#'                          eliminar.na = TRUE,
#'                          grafico = FALSE,
#'                          exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe. Si el dataframe tiene más de una variable, solicitará al usuario que idenfique el nombre de la variable para la que se quiere calcular la tabla de frecuencias.
#' @param eliminar.na Valor lógico. Por defecto \code{eliminar.na = TRUE}. Si se quiere obtener la tabla de frecuencias con NAs, cambiar el argumento a \code{eliminar.na = FALSE}.
#' @param grafico Si \code{grafico = TRUE}, representa el histograma o el gráfico de barras de la variable seleccionada.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Devuelve la tabla de frecuencias como una \code{tibble}. Si \code{grafico = TRUE}, se devuelve en una lista la tabla de frecuencias y su representación gráfica.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom tidyr pivot_longer drop_na
#' @import dplyr
#'
#' @export
tabla.frecuencias <- function(x,
                              eliminar.na = TRUE,
                              grafico = FALSE,
                              exportar = FALSE){

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
      stop("El nombre de la variable no es v\u00e1lido")
    }
  }

  x <- as.data.frame(x) %>%
    dplyr::select(all_of(variable))

  y <- varnames[variable] # nombre de la variable seleccionada

  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer","factor","logic")) {
    stop("No puede construirse la tabla de frecuencias, la variable que has\n
         seleccionado es car\u00e1cter")
  }

  if(length(x) > 1){
    stop("Esta funci\u00f3n solo puede contruir la tabla de frecuencias de una variable")
    print("Para obtener la tabla de frecuencias de mas de una variable utiliza la funci\u00f3n apply")
  }

  valores_distintos <- nrow(unique(x))
  valores_ordenados <- unique(x)
  valores_ordenados <- valores_ordenados[,1]

  if(valores_distintos > 20){

    # tabla de frecuencias de valores agrupados

    print("La variable presenta muchos valores distintos (mas de 20)")
    agrupar <- as.numeric(readline('\u00bfQuieres agrupar los valores en intervalos?: \n 1. "S\u00ed, agrupar en intervalos." \n 2. "No, usar los valores sin agrupar." \n'))

    if(agrupar == 1){
      clase <- sapply(x, class)

      if(clase %in% c("factor","character","logic")){
        stop("La variable no es cuantitativa, no puede representarse el histograma")
      }

      if(nrow(x)<=100){
        intervalos <- ceiling(sqrt(nrow(x)))
      } else {
        intervalos <- ceiling(log(nrow(x))/log(2) + 1)
      }

      amplitud <- (max(x)-min(x))/intervalos

      x$cut <- cut(x[,1], seq(min(x[,1]),max(x[,1]),amplitud),
                   include.lowest = TRUE,
                   dig.lab = 8)

      x <- x[2]
      names(x) <- y

    } else{

      print("Has decidido no agrupar los valores, puede que el diagrama de barras no represente adecuadamente la distribuci\u00f3n de la variable")

      agrupar = 0

    }


  } else{

    print("Has decidido no agrupar los valores.")

    agrupar = 0

    }

  # tabla de frecuencias

  # ordena de menor a mayor los valores
  tabla <- x %>% dplyr::arrange(x) %>%
    dplyr::group_by_at(y) %>%   # en lugar de group_by(.dots=y)
    dplyr::count() %>%
    dplyr::ungroup()

  names(tabla) <- c(y,"ni") # nombres de las columnas de tabla

  # calcula las frecuencias
  tabla <- tabla %>%
    dplyr::mutate(Ni = cumsum(ni),
                  fi = ni / sum(ni),
                  Fi = cumsum(fi))

  if(eliminar.na == TRUE){
    x <- drop_na(x)

    tabla <- x %>% dplyr::arrange(x) %>%
      dplyr::group_by_at(y) %>%
      dplyr::count() %>%
      dplyr::ungroup()

    names(tabla) <- c(y,"ni")

    tabla <- tabla %>%
      dplyr::mutate(Ni = cumsum(ni),
                    fi = ni / sum(ni),
                    Fi = cumsum(fi))

  } else {

    tabla <- x %>% dplyr::arrange(x) %>%
      dplyr::group_by_at(y) %>%
      dplyr::count() %>%
      dplyr::ungroup()

    names(tabla) <- c(y,"ni")

    tabla <- tabla %>%
      dplyr::mutate(Ni = cumsum(ni),
                    fi = ni / sum(ni),
                    Fi = cumsum(fi))

  }

  if (exportar) {
    filename <- paste("Tabla de frecuencias de ", y, " (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(tabla, file = filename)
  }

  if(grafico){

    df <- cbind(tabla[1],tabla[2],tabla[4],tabla[3],tabla[5])

    if(agrupar == 1){

      marca_clase <- function(cut_label) {
        mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
      }

      df$marca <- sapply(df[,1], marca_clase)
      variables <- names(df)

      print("Los valores han sido agrupados en intervalos y se representar\u00e1n mediante un histograma")

      plot <- ggplot(df, aes_string(x=variables[1], y = variables[2])) +
        geom_bar(stat = "identity", width = 1,
                 color = "white", fill = "orange") +
        geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
        geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.5, size = 2.5, color = "darkgreen" ) +
        labs(title = paste("Histograma de ",variables[1],sep=""),
               x = variables[1],
               y = "") +
        theme(
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          legend.background = element_blank(), # get rid of legend bg
          legend.box.background = element_blank(), # get rid of legend panel bg
          legend.key = element_blank(), # get rid of key legend fill, and of the surrounding
          axis.ticks.x=element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(size=6,angle=45),
          axis.text.y=element_blank(),
          legend.title = element_blank()
        )


    } else {

      variables <- names(df)

      if(valores_distintos > 10){

        print("El diagrama de barras puede no ser una buena representaci\u00f3n gr\u00e1fica si la variable presenta muchos distintos valores (aconsejable como m\u00e1ximo 10-15)")


        plot <- ggplot(df, aes_string(x=as.factor(round(df[,1],2)),y=variables[2])) +
          geom_bar(stat = "identity",  fill = "orange") +
          geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
          geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.65, size = 2.5, color = "blue" ) +
          labs(title = paste("Diagrama de barras de ", variables[1], sep=""),
               x = variables[1],
               y = "") +
          theme(
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            legend.background = element_blank(), # get rid of legend bg
            legend.box.background = element_blank(), # get rid of legend panel bg
            legend.key = element_blank(), # get rid of key legend fill, and of the surrounding
            axis.ticks.x=element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_text(size=6,angle=90),
            axis.text.y=element_blank(),
            legend.title = element_blank()
          )


      } else {

        plot <- ggplot(df, aes_string(x=variables[1],y=variables[2])) +
          geom_bar(stat = "identity", width = 0.5, fill = "orange") +
          geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
          geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.65, size = 2.5, color = "blue" ) +
          labs(title = paste("Diagrama de barras de ", variables[1], sep=""),
               x = variables[1],
               y = "") +
          scale_x_continuous(breaks = valores_ordenados) +
          theme(
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank(), # get rid of major grid
            panel.grid.minor = element_blank(), # get rid of minor grid
            legend.background = element_blank(), # get rid of legend bg
            legend.box.background = element_blank(), # get rid of legend panel bg
            legend.key = element_blank(), # get rid of key legend fill, and of the surrounding
            axis.ticks.x=element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            legend.title = element_blank()
          )
      }



    }

  } # cierra grafico

  if(grafico){

    return(list(tabla,plot))

  } else{

    return(tabla)

  }

}
