#' @title Tabla de frecuencias.
#'
#' @description Esta función presenta la distribución de frecuencias de una variable.
#' @usage tabla.frecuencias(x,
#' eliminar.na = TRUE,
#' grafico = FALSE,
#' exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe. Si el dataframe tiene más de una variable, solicitará al usuario que idenfique el nombre de la variable para la que se quiere calcular la tabla de frecuencias.
#' @param eliminar.na Valor lógico. Por defecto eliminar.na = TRUE. Si se quiere obtener la tabla de frecuencias con NAs, cambiar el argumento a FALSE.
#' @param grafico Si grafico = TRUE, representa el histograma o el gráfico de barras de la variable seleccionada.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @import tidyverse
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
      stop("El nombre de la variable no es valido")
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

  if(valores_distintos > 20){

    # tabla de frecuencias de valores agrupados

    print("La variable presenta muchos valores distintos (mas de 20) y se agrupar\u00e1n en intervalos.")

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

    if(valores_distintos > 20){

      marca_clase <- function(cut_label) {
        mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
      }

      df$marca <- sapply(df[,1], marca_clase)
      variables <- names(df)

      print("La variable presenta muchos valores distintos (mas de 20) y se representar\u00e1 mediante un histograma")

      plot <- ggplot(df, aes_string(x=variables[1], y = variables[2])) +
        geom_bar(stat = "identity", width = 1,
                 color = "white", fill = "orange") +
        geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
        geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.5, size = 2.5, color = "darkgreen" ) +
        labs(title = paste("Histograma de ",variables[1],sep=""),
               x = variables[1],
               y = "") +
        tema_blanco


    } else {

      print("El diagrama de barras puede ser una buena representaci\u00f3n gr\u00e1fica si la variable no presenta muchos distintos valores (aconsejable como m\u00e1ximo 10)")

      variables <- names(df)

      if(valores_distintos > 10){
        anchura = NULL
      } else {
        anchura = 0.5
      }

      plot <- ggplot(df, aes_string(x=variables[1],y=variables[2])) +
        geom_bar( stat = "identity", width = anchura, fill = "orange") +
        geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
        geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.65, size = 2.5, color = "darkgreen" ) +
        labs(title = paste("Diagrama de barras de ", variables[1], sep=""),
             x = variables[1],
             y = "") +
        tema_blanco

    }

  }

  if(grafico){

    return(list(tabla,plot))

  } else{

    return(tabla)

  }

}
