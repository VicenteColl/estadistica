#' @title Tabla de frecuencias.
#'
#' @description Esta función presenta la distribución de frecuencias de una variable.
#' @usage tabla.frecuencias(x, eliminar.na = TRUE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param eliminar.na Valor lógico. Por defecto eliminar.na = TRUE. Si se quiere obtener la tabla de frecuencias con NAs, cambiar el argumento a FALSE.
#'
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
#' Esteban García, J. et al. (2005). Estadística descriptiva y nociones de probabilidad. Thomson.
#'
#' @import tidyverse
#'
#' @export
tabla.frecuencias <- function(x, eliminar.na = TRUE){

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
         seleccionado es caracter")
  }

  if(length(x) > 1){
    stop("Esta funcion solo puede contruir la tabla de frecuencias de una variable")
  }

  tabla <- x %>% dplyr::arrange(x) %>%
    dplyr::group_by(.dots=y) %>%
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
      dplyr::group_by(.dots=y) %>%
      dplyr::count() %>%
      dplyr::ungroup()

    names(tabla) <- c(y,"ni")

    tabla <- tabla %>%
      dplyr::mutate(Ni = cumsum(ni),
                    fi = ni / sum(ni),
                    Fi = cumsum(fi))

  } else {

    tabla <- x %>% dplyr::arrange(x) %>%
      dplyr::group_by(.dots=y) %>%
      dplyr::count() %>%
      dplyr::ungroup()

    names(tabla) <- c(y,"ni")

    tabla <- tabla %>%
      dplyr::mutate(Ni = cumsum(ni),
                    fi = ni / sum(ni),
                    Fi = cumsum(fi))

  }

  return(tabla)

}
