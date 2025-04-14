#' @title Varianza.
#'
#' @description Calcula la varianza.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#' @usage varianza(x,
#'        variable = NULL,
#'        pesos = NULL,
#'        tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto de calcula la varianza muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasivarianza muestral.
#'
#' @return Esta función devuelve un objeto de la clase \code{vector}. Si \code{tipo="muestral"}, devuelve la varianza muestral. Si \code{tipo="cuasi"}, devuelve la cuasi-varianza muestral.
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
#' @details
#'
#' (1) La expresión de la varianza muestral es:
#'
#' \if{html}{\figure{varianza.png}{options: width="40\%" alt="Figure: varianza.png"}}
#' \if{latex}{\figure{varianza.png}{options: width=5cm}}
#'
#' La varianza muestral así definida es el estimador máximo verosímil de la varianza de una población normal
#'
#' (2) Muchos manuales y prácticamente todos los softwares (SPSS, Excel, etc.) calculan la expresión:
#'
#' \if{html}{\figure{cuasivarianza.png}{options: width="40\%" alt="Figure: cuasivarianza.png"}}
#' \if{latex}{\figure{cuasivarianza.png}{options: width=5cm}}
#'
#' Nosotros llamamos a esta medida: cuasi-varianza muestral y es un estimador insesgado de la varianza poblacional.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la varianza poblacional:
#'
#'
#' \if{html}{\figure{varianzapob.png}{options: width="40\%" alt="Figure: varianzapob.png"}}
#' \if{latex}{\figure{varianzapob.png}{options: width=5cm}}
#'
#' @seealso \code{\link{media}}, \code{\link{desviacion}}, \code{\link{coeficiente.variacion}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' varianza1 <- varianza(startup[1])
#' varianza2 <- varianza(startup,variable=1)
#' varianza3 <- varianza(startup,variable=1, tipo="cuasi")
#'
#' @importFrom stats var
#'
#' @export
varianza <- function(x, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {

  # Capturar nombre original
  var_name <- if(is.name(substitute(x))) deparse(substitute(x)) else "variable"

  # Capturar el nombre original si es un vector
  var_name <- deparse(substitute(x))

  # Manejo de nombres para diferentes tipos de entrada
  if (is.data.frame(x) || is.list(x)) {
    # Para data.frames/listas
    original_names <- names(x)
    x <- as.data.frame(x)

    if (is.null(variable)) {
      varnames <- names(x)[sapply(x, is.numeric)]
    } else {
      if (is.numeric(variable)) {
        varnames <- names(x)[variable]
      } else {
        varnames <- variable
      }
    }
  } else {
    # Para vectores
    if (grepl("\\$", var_name)) {
      # Si es de tipo dataframe$columna
      varnames <- sub(".*\\$", "", var_name)
    } else {
      # Si es un vector simple
      varnames <- "variable"
    }
    x <- data.frame(x)
    names(x) <- varnames
    original_names <- varnames
  }

  # Validar tipo de varianza
  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))


  # Selección de variables
  if(!is.null(variable)) {
    if(is.character(variable)) {
      if(!all(variable %in% names(x))) stop("Nombre de variable no v\u00e1lido")
      x <- x[, variable, drop = FALSE]
    } else if(is.numeric(variable)) {
      if(any(variable > ncol(x))) stop("Selecci\u00f3n err\u00f3nea de variables")
      x <- x[, variable, drop = FALSE]
    } else {
      stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }
  } else {
    # Seleccionar solo columnas numéricas
    x <- x[, sapply(x, is.numeric), drop = FALSE]
    if(ncol(x) == 0) stop("No hay variables num\u00e9ricas para calcular")
  }

  # Validar tipos numéricos
  if(!all(sapply(x, is.numeric))) {
    stop("Alguna variable seleccionada no es num\u00e9rica")
  }

  # Manejo de pesos
  if(!is.null(pesos)) {
    if(ncol(x) > 1) {
      stop("Solo puedes seleccionar una variable con pesos")
    }

    if(is.character(pesos)) {
      if(!pesos %in% names(x)) stop("Nombre de pesos no v\u00e1lido")
        pesos_col <- pesos
    } else if(is.numeric(pesos)) {
      if(pesos > ncol(x)) stop("Índice de pesos inv\u00e1lido")
        pesos_col <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    # Calcular varianza ponderada
    datos <- na.omit(x)
    if(nrow(datos) < 2) return(NA_real_)

    media_pond <- sum(datos[[1]] * datos[[pesos_col]]) / sum(datos[[pesos_col]])
    sum_cuad <- sum((datos[[1]] - media_pond)^2 * datos[[pesos_col]])

    if(tipo == "muestral") {
      result <- sum_cuad / sum(datos[[pesos_col]])
    } else {
      result <- sum_cuad / (sum(datos[[pesos_col]]) - 1)
    }

    names(result) <- paste0("varianza_", names(x)[1])
    return(round(result, 4))
  }

  # Calcular varianza simple
  n <- nrow(x)
  if(n < 2) return(NA_real_)

  if(tipo == "muestral") {
    factor <- (n - 1)/n
  } else {
    factor <- 1
  }

  result <- factor * sapply(x, stats::var, na.rm = TRUE)
  names(result) <- paste0("varianza_", names(x))
  return(round(result, 4))
}
