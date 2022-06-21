#' @title Tabla doble entrada.
#'
#' @description Calcula la tabla de frecuencias bidimensionales.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrtablabidimensional.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrtablabidimensional.png}{options: width=3cm}}
#'
#' @usage tabla.bidimensional(x,
#' var_filas = NULL,
#' var_columnas = NULL,
#' distribucion = c("cruzada","condicionada"),
#' frecuencias = c("absolutas","relativas"),
#' exportar = FALSE)
#'
#' @param x Conjunto de datos. Tiene que ser un dataframe (al menos dos variables, es decir, dos columnas).
#' @param var_filas Variable fila.Por defecto su valor es NUll y el sistema pedirá que se identifique. Sin embargo, el usuario puede escribir el nombre o posición de la variable cuyos valores quiere representar por filas.
#' @param var_columnas Variable columna. Por defecto su valor es NUll y el sistema pedirá que se identifique. Sin embargo, el usuario puede escribir el nombre o posición de la variable cuyos valores quiere representar por columnas
#' @param distribucion Es un caracter. Por defecto se obtien la tabla cruzada (\code{distribucion = "cruzada"}). Para obtener las distribuciones condicionadas cambiar a \code{distribucion = "condicionada"}.
#' @param frecuencias Es un carácter. Por defecto se obtienen las frecuencias absolutas ordinarias (\code{frecuencias = "absolutas"}). Para obtener las frecuencias relativas ordinarias cambiar a \code{frecuencias = "relativas"}.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Devuelve la tabla cruzada de las dos variables seleccionadas en un \code{data.frame}
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
#' @importFrom stats addmargins na.omit
#' @import tidyverse
#'
#' @export
tabla.bidimensional <- function(x,
                                var_filas = NULL,
                                var_columnas = NULL,
                                distribucion = c("cruzada","condicionada"),
                                frecuencias = c("absolutas","relativas"),
                                exportar = FALSE){

  distribucion <- tolower(distribucion)
  distribucion <- match.arg(distribucion)

  frecuencias <- tolower(frecuencias)
  frecuencias <- match.arg(frecuencias)

  x <- data.frame(x)
  varnames <- names(x)

  if(length(x)<2){
    stop("El conjunto de datos seleccionada solo tiene una variable.")
  }

  if(is.null(var_filas) | is.null(var_columnas)){
    stop("Debes seleccionar la variable fila y columna")
  }

  if(is.numeric(var_filas)){
    if(var_filas<=length(x)){
      var_filas <- var_filas}
    else{
      stop("Selecci\u00f3n err\u00f3nea de variable")
    }
  }

  if(is.character(var_filas)){
    if(var_filas %in% varnames){
      var_filas = match(var_filas,varnames)
    } else {
      stop("El nombre de la variable por filas no es v\u00e1lido")
    }
  }


  if(is.numeric(var_columnas)){
    if(var_columnas<=length(x)){
      var_columnas <- var_columnas}
    else{
      stop("Selecci\u00f3n err\u00f3nea de variable")
    }
  }

  if(is.character(var_columnas)){
    if(var_columnas %in% varnames){
      var_columnas = match(var_columnas,varnames)
    } else {
      stop("El nombre de la variable por columna no es v\u00e1lido")
    }
  }

  if(var_filas == var_columnas){
    stop("La variable por fila y columna es la misma variable")
  }

  variable <- c(var_filas,var_columnas)


  x <- x[,variable] %>% as.data.frame()
  names(x) <- varnames[variable]
  varnames <- names(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer","factor","logic"))){
    stop("No puede construirse la tabla de frecuencias, alguna variable seleccionada es car\u00e1cter")
  }


  if(frecuencias == "absolutas"){

    if(distribucion == "cruzada"){
      tabla <- x %>%
        table()
      tabla <- addmargins(tabla)

    } else{
      print("Si quieres obtener la distribuciones de variable1/variable2 (condici\u00f3n por columnas) introduce el valor 1, en caso contrario variable2/variable1 (condici\u00f3n por filas) introduce el valor 2")
      tipo <- readline(prompt = "Distribuci\u00f3nes condicionadas por columnas (1) o por filas (2): ")
      tipo = as.numeric(tipo)

      tabla2 <- x %>%
        table()
      namesfilas <- row.names(tabla2)
      namescolumnas <- colnames(tabla2)

      if(tipo == 1){
        tabla_aux <- x %>%
          select(tipo) %>%
          group_by(filas) %>%
          count() %>%
          ungroup() %>%
          select(n)
        n = length(namescolumnas)
        tabla_aux <- cbind(tabla_aux, replicate(n-1,tabla_aux$n))

      } else{
        tabla_aux <- x %>%
          select(tipo) %>%
          group_by(columnas) %>%
          count() %>%
          ungroup() %>%
          select(n)

        n = length(namesfilas)
        tabla_aux <- cbind(tabla_aux, replicate(n-1,tabla_aux$n)) %>%
          t()
      }

      tabla <-  tabla_aux * as.matrix(prop.table(tabla2,tipo))
      row.names(tabla) <- namesfilas
      colnames(tabla) <- namescolumnas

    }

  } else{

    if(distribucion == "cruzada"){
      tabla <- x %>%
        table()
      tabla <- prop.table(tabla)
      tabla <- addmargins(tabla)

    } else{
      print("Si quieres obtener la distribuci\u00f3nes de varible1/variable2 (por filas) introduce el valor 1, en caso contrario variable2/variable1 (por columnas) introduce el valor 2")
      tipo <- readline(prompt = "Distribuci\u00f3nes condicionadas por filas (1) o por columnas (2): ")
      tipo = as.numeric(tipo)

      if(tipo == 1){
        tabla2 <- x %>%
          table()
        tabla <- prop.table(tabla2,2)
      } else{
        tabla2 <- x %>%
          table()
        tabla <- prop.table(tabla2,1)
      }
    }
  }

  tabla <- as.data.frame.matrix(tabla)


  if (exportar) {
    filename <- paste("Tabla cruzada de ", variable[1]," y ", variable[2], " (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(tabla, rowNames = TRUE, file = filename)
  }

  return(tabla)

}
