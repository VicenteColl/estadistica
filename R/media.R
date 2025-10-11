#' @title Media (aritmética).
#'
#' @description Calcula la media aritmética.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrposicion.png}{options: width=3cm}}
#'
#' @usage media(x, variable = NULL, pesos = NULL)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#'
#' @return Si \code{pesos = NULL}, devuelve la media (aritmética) de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la media de la variable para la que se ha facilitado la distribución de frecuencias.
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
#' Si se obtiene la media (muestral) a partir de los datos brutos, como generalmente hacen los softwares:
#'
#' \if{html}{\figure{media.png}{options: width="80" alt="Figure: media.png"}}
#' \if{latex}{\figure{media.png}{options: width=3.5cm}}
#'
#' Si se desea obtener la media (muestral) a partir de una tabla estadística se utiliza la expresión:
#'
#' \if{html}{\figure{media2.png}{options: width="80" alt="Figure: media2.png"}}
#' \if{latex}{\figure{media2.png}{options: width=3.5cm}}
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la media poblacional:
#'
#' \if{html}{\figure{mediapob.png}{options: width="80" alt="Figure: mediapob.png"}}
#' \if{latex}{\figure{mediapob.png}{options: width=3cm}}
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
#' media1 <- media(startup[1])
#' media2 <- media(startup,variable=1)
#' media3 <- media(salarios2018,variable=6,pesos=7)
#'
#' @importFrom stats na.omit
#'
#' @import dplyr
#'
#' @export
media <- function(x,
                  variable = NULL,
                  pesos = NULL) {

  # Si es un vector, convertirlo a data.frame
  if (!is.data.frame(x)) {
    x <- data.frame(variable = x)
  }

  # Determinar variables seleccionadas
  if (is.null(variable)) {
    varnames <- names(x)[sapply(x, is.numeric)]
  } else if (is.numeric(variable)) {
    varnames <- names(x)[variable]
  } else if (is.character(variable)) {
    varnames <- variable
  } else {
    stop("El argumento 'variable' debe ser num\u00e9rico o de tipo car\u00e1cter")
  }

  # Verificar que las columnas seleccionadas existen
  if (!all(varnames %in% names(x))) {
    stop("Alguna variable seleccionada no existe en el data.frame")
  }

  # Subconjunto con las variables seleccionadas
  x_sel <- x[, varnames, drop = FALSE]

  # Manejo de pesos (si existen)
  if (!is.null(pesos)) {
    if (length(varnames) > 1 || length(pesos) > 1) {
      stop("Para el cl\u00e1culo ponderado solo puedes seleccionar una variable y un vector de pesos")
    }

    if (is.character(pesos)) {
      pesos_name <- pesos
    } else if (is.numeric(pesos)) {
      pesos_name <- names(x)[pesos]
    } else {
      stop("El argumento 'pesos' debe ser num\u00e9rico o de tipo car\u00e1cter")
    }

    if (!pesos_name %in% names(x)) {
      stop("El nombre de los pesos no es v\u00e1lido")
    }

    if (pesos_name == varnames) {
      stop("No puedes usar la misma variable como dato y como peso")
    }

    x_sel <- data.frame(variable = x[[varnames]], pesos = x[[pesos_name]])
  }

  # Comprobacion tipo de variable
  if (!all(sapply(x_sel, is.numeric))) {
    stop("No puede calcularse la media: alguna variable seleccionada no es cuantitativa")
  }

  # Calculo de la media
  if (is.null(pesos)) {
    result <- sapply(x_sel, mean, na.rm = TRUE)
    names(result) <- paste0("media_", names(x_sel))
  } else {
    result <- sum(x_sel[[1]] * x_sel[[2]], na.rm = TRUE) / sum(x_sel[[2]], na.rm = TRUE)
    names(result) <- paste0("media_", varnames)
  }

  # --- Convertir a data.frame con una fila ---
  media_df <- as.data.frame(t(result))
  names(media_df) <- varnames

  class(media_df) <- c("resumen", class(media_df))


  return(media_df)

}
