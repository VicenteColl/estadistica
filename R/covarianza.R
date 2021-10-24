#' @title Covarianza.
#'
#' @description Calcula la covarianza.
#' @usage covarianza(x,
#'                   variable = NULL,
#'                   tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x solo tiene 2 variables (columnas), el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de las variables a seleccionar.
#' @param tipo Es un carácter. Por defecto de calcula la covarianza muestral (tipo = "muestral"). Si tipo = "cuasi", se calcula la cuasi-covarianza muestral.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' (1) La covarianza muestral se obtiene a partir de la siguiente expresión:
#'
#' \figure{covarianza_muestra.png}{options: width="50\%" heigth="50\%"}
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la covarianza a partir de la expresión:
#'
#'\figure{covarianza_cuasi.png}{options: width="50\%" heigth="50\%"}
#'
#' Nosotros nos referimos a esta expresión como cuasi-covarianza muestral.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la covarianza poblacional:
#'
#' \figure{covarianza_pob.png}{options: width="50\%" heigth="50\%"}
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
#' @importFrom stats cov
#' @import dplyr
#'
#' @export
covarianza <- function(x, variable = NULL, tipo = c("muestral","cuasi")){

  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  if(is.null(variable)){

    if(length(x) == 2){

      x <- data.frame(x)
      varnames <- names(x)

    } else{

      warning("Para obtener la matriz de varianzas-covarianzas utilizar la funcion matriz.var.covar()")
      stop("El conjunto de datos seleccionado tiene mas de 2 variables.")

    }

  } else if(length(variable) == 2){

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable

      } else{

        stop("Seleccion errronea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){

        variable = match(variable,varnames)

      } else {

        stop("El nombre de la variable no es valido")

      }

    }

    x <- x[,variable] %>% as.data.frame()
    varnames <- names(x)

  } else{

    warning("Para obtener la matriz de correlacion utilizar la funcion matriz.cor")
    stop("Para calcular la correlacion solo puedes seleccionar dos variables")

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {

    stop("No puede calcularse la covarianza, alguna variable que has seleccionado no es cuantitativa")

  }

  if(tipo == "muestral"){

    n <- nrow(x)
    factor = (n-1)/n

    covarianza <- factor * cov(x[1],x[2], use ="everything")


  } else{

    covarianza <- cov(x[1],x[2], use ="everything")

  }

  covarianza <- as.data.frame(covarianza)

  names(covarianza) <- paste("covarianza_",varnames[1],"_",varnames[2],sep="")
  row.names(covarianza) <- NULL

  return(covarianza)

}
