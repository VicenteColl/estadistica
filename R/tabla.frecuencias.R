#' @title Tabla de frecuencias.
#'
#' @description Esta función presenta la distribución de frecuencias de una variable.
#' @usage tabla.frecuencias(x, eliminar.na = TRUE, exportar = TRUE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe. Si el dataframe tiene más de una variable, solicitará al usuario que idenfique el nombre de la variable para la que se quiere calcular la tabla de frecuencias.
#' @param eliminar.na Valor lógico. Por defecto eliminar.na = TRUE. Si se quiere obtener la tabla de frecuencias con NAs, cambiar el argumento a FALSE.
#' @param exportar Por defecto, los resultados se exportan a una hoja de cálculo Excel (exportar = TRUE).

#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Olga Blasco-Blasco} (\email{olga.blasco@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#' Universidad de Valencia (España)
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
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
tabla.frecuencias <- function(x, eliminar.na = TRUE, exportar = TRUE){

  x <- as.data.frame(x)

  varnames <- colnames(x)
  numvariables <- length(x)

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

  y <- varnames[variable]

  clase <- sapply(x, class)

  if (!clase %in% c("numeric","integer","factor","logic")) {
    stop("No puede construirse la tabla de frecuencias, la variable que has\n
         seleccionado es car\u00e1cter")
  }

  if(length(x) > 1){
    stop("Esta funci\u00f3n solo puede contruir la tabla de frecuencias de una variable")
    print("Para obtener la tabla de frecuencias de mas de una variable utiliza la funci\u00f3n apply")
  }

  tabla <- x %>% dplyr::arrange(x) %>%
    dplyr::group_by_at(y) %>%   # en lugar de group_by(.dots=y)
    dplyr::count() %>%
    dplyr::ungroup()

  names(tabla) <- c(y,"ni")

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


  return(tabla)

}
