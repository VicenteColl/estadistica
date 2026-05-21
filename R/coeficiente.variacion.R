#' @encoding UTF-8
#' @title Coeficiente de variación.
#'
#' @description Calcula el coeficiente de variación de Pearson.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdispersion.png}{options: style="width: 25\%;"}}
#' \if{latex}{\figure{qrdispersion.png}{options: width=3cm}}
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
#' \if{html}{\figure{coeficientevariacion.png}{options: style="width: 20\%;"}}
#' \if{latex}{\figure{coeficientevariacion.png}{options: width=2cm}}
#'
#' donde S es la desviación típica muestral. También puede calcularse utilizando la cuasi-desviación típica (Sc).
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N), se obtiene el coeficiente de variación poblacional:
#'
#' \if{html}{\figure{coeficientevariacionpob.png}{options: style="width: 20\%;"}}
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
coeficiente.variacion <- function(data, variable = NULL, pesos = NULL, tipo = c("muestral", "cuasi")) {

  tipo <- match.arg(tolower(tipo), c("muestral", "cuasi"))

  # Si no es data.frame, convertir (vector simple)
  if (!is.data.frame(data)) {
    data <- data.frame(variable = data)
  }

  # Capturar argumentos para pasarlos a las funciones internas
  var_quo <- enquo(variable)
  pesos_quo <- enquo(pesos)

  # Calcular la media
  medias <- media(data, variable = !!var_quo, pesos = !!pesos_quo)

  # Calcular la desviación
  desviaciones <- desviacion(data, variable = !!var_quo, pesos = !!pesos_quo, tipo = tipo)

  # Obtener los nombres de las columnas de grupo (si las hay)
  grupos <- dplyr::group_vars(data)

  # Combinar medias y desviaciones
  # Ambas tienen las mismas columnas de grupo y las mismas columnas de variables
  resultado <- medias

  # Para cada variable numérica (excepto grupos), calcular CV = desv / media
  vars_numericas <- setdiff(names(medias), grupos)

  for (var in vars_numericas) {
    # Asegurar que la desviación existe
    if (var %in% names(desviaciones)) {
      resultado[[var]] <- round(desviaciones[[var]] / medias[[var]], 4)
    } else {
      warning(paste("No se encontró desviación para", var))
    }
  }

  # Quedarse solo con las columnas de grupo y los coeficientes
  resultado <- resultado[, c(grupos, vars_numericas), drop = FALSE]

  class(resultado) <- c("resumen", class(resultado))
  return(resultado)
}
