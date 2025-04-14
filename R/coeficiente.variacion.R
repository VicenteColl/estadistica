#' @encoding UTF-8
#' @title Coeficiente de variación.
#'
#' @description Calcula el coeficiente de variación de Pearson.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
#'
#' @usage coeficiente.variacion(x,
#'           variable = NULL,
#'           pesos = NULL,
#'           tipo = c("muestral","cuasi"))
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param tipo Es un carácter. Por defecto calcula la desviación típica muestral (\code{tipo = "muestral"}). Si \code{tipo = "cuasi"}, se calcula la cuasi-desviación típica muestral.
#'
#' @return Esta función devuelve el valor del coeficiente de variación en un objeto de la clase \code{vector}. Por defecto, el coeficiente de variación se calcula utilizando la desviación típica muestral.
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
#' El coeficiente de variación (muestral) se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{coeficientevariacion.png}{options: width="20\%" alt="Figure: coeficientevariacion.png"}}
#' \if{latex}{\figure{coeficientevariacion.png}{options: width=2cm}}
#'
#' donde S es la desviación típica muestral. También puede calcularse utilizando la cuasi-desviación típica (Sc).
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N), se obtiene el coeficiente de variación poblacional:
#'
#' \if{html}{\figure{coeficientevariacionpob.png}{options: width="20\%" alt="Figure: coeficientevariacionpob.png"}}
#' \if{latex}{\figure{coeficientevariacionpob.png}{options: width=2cm}}
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
#' variacion1 <- coeficiente.variacion(startup[1])
#' variacion2 <- coeficiente.variacion(startup)
#'
#' @import dplyr
#'
#' @export
coeficiente.variacion <- function(x,
                                  variable = NULL,
                                  pesos = NULL,
                                  tipo = c("muestral", "cuasi")) {

  tipo <- tolower(tipo)
  tipo <- match.arg(tipo)

  # Capturar el nombre original si es un vector
  var_name <- deparse(substitute(x))

  # Si es un data.frame/lista y no se especifica variable, usar nombres de columnas
  if (is.data.frame(x) || is.list(x)) {
    if (is.null(variable)) {
      varnames <- names(x)[sapply(x, is.numeric)]
    } else {
      if (is.numeric(variable)) {
        varnames <- names(x)[variable]
      } else {
        varnames <- variable
      }
    }
    x <- as.data.frame(x)
  } else {
    # Si es un vector, usar el nombre capturado
    varnames <- ifelse(grepl("\\$", var_name),
                       sub(".*\\$", "", var_name),
                       "variable")
    x <- data.frame(x)
    names(x) <- varnames
  }

  # Manejo de pesos
  if(!is.null(pesos)) {
    if(length(varnames) > 1 || (length(pesos) > 1)) {
      stop("Para calcular el coeficiente de variaci\u00f3n a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")
    }

    if(is.character(pesos)) {
      if(!pesos %in% names(x)) {
        stop("El nombre de los pesos no es v\u00e1lido")
      }
      pesos_name <- pesos
    } else if(is.numeric(pesos)) {
      pesos_name <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    # Conservar el nombre de la variable principal
    main_var_name <- varnames[1]
    x <- x[, c(main_var_name, pesos_name), drop = FALSE]
    varnames <- main_var_name
  }

  # Verificar que todas las variables sean cuantitativas
  if(!all(sapply(x, is.numeric))) {
    stop("No puede calcularse el coeficiente de variaci\u00f3n, alguna variable que has seleccionado no es cuantitativa")
  }

  # Cálculo del coeficiente de variación
  if(is.null(pesos)) {
    valor_media <- media(x)

    if(tipo == "muestral") {
      valor_desviacion <- desviacion(x)
    } else {
      valor_desviacion <- desviacion(x, tipo = "cuasi")
    }

    coef_variacion <- valor_desviacion / valor_media
    names(coef_variacion) <- paste0("coef_variacion_", names(x))

  } else {
    valor_media <- media(x, variable = 1, pesos = 2)

    if(tipo == "muestral") {
      valor_desviacion <- desviacion(x, variable = 1, pesos = 2)
    } else {
      valor_desviacion <- desviacion(x, variable = 1, pesos = 2, tipo = "cuasi")
    }

    coef_variacion <- valor_desviacion / valor_media
    names(coef_variacion) <- paste0("coef_variacion_", varnames[1])
  }

  return(round(coef_variacion, 4))
}
