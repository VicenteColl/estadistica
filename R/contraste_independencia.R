#' @title Contraste de Independencia.
#'
#' @description Contrasta la hipótesis de independencia entre las categorías de dos variables categóricas o criterios de clasificación en los que se encuentra clasificada la muestra proveniente de dicha población bidimensional.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcmedia.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcmedia.png}{options: width=3cm}}

#' @usage contraste_independencia(x,
#' introducir = FALSE,
#' alfa = 0.05,
#' grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser una matriz o un dataframe. Debe contener sólo 2 columnas.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre el número de categorías de cada una de las variables, el nombre de cada categoría de la variable fila y de la variable columna. A continuación se abrirá una ventana con un editor de datos y deberá introducir los valores de las frecuencias observadas de la tabla de contingencia.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}.
#'
#' @return La función devuelve un objeto de la clase \code{list}. La lista contendrá información sobre: la hipótesis nula contrastada, el estadístico de prueba y el p-valor. Si \code{grafico=TRUE} se incluirá una representación gráfica de la región de aceptación-rechazo con el valor crítico.
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
#' (1) El estadístico del contraste de independencia es:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{I} \sum_{j=1}^{J}
#' \frac{(O_{ij} - E_{ij})^{2}}{E_{ij}} =
#' \sum_{i=1}^{I} \sum_{j=1}^{J}
#' \frac{(O_{ij} - \frac{O_{i.} \times O_{.j}}{n})^{2}}{\frac{O_{i.} \times O_{.j}}{n}}}
#'
#' donde \(O_{ij}\) son las frecuencias conjuntas observadas, \(E_{ij}\) son las frecuencias teóricas o esperadas,
#' \(O_{i.}\) y \(O_{.j}\) son las frecuencias marginales de cada variable, y \(n\) es el tamaño de la muestra.
#'
#' \deqn{\chi_{(I-1)(J-1)}^{2}}
#'
#' donde I y J son el número de categorías de la variable fila y de la variable columna, respectivamente.
#'
#' Además, se exige que el tamaño de la muestra sea grande y que todas las frecuencias teóricas no estén por debajo de 5.
#' Si alguna no lo cumple, es necesario reagrupar las categorías contiguas hasta conseguir superar esa cota.
#'
#' (2) Si el número de grados de libertad es 1, al estadístico del contraste se le aplica la siguiente corrección de Yates:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{k}
#' \frac{(\left| O_{i} - E_{i} \right| - 0.5)^{2}}{E_{i}}}
#'
#'
#' @seealso \code{\link{contraste_homogeneidad}},\code{\link{contraste_bondad_cat}}
#'
#' @references
#' Casas José M. (1997) Inferencia estadística. Editorial: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' @importFrom utils edit
#' @import dplyr ggplot2
  #'
  #' @export

contraste_independencia <- function(x,
                                    introducir = FALSE,
                                    alfa = 0.05,
                                    grafico = FALSE) {


  # Si el usuario quiere introducir los datos manualmente
  if (introducir == TRUE) {

    # Caso cuando el usuario quiere introducir los datos manualmente
    nfilas <- as.numeric(readline(prompt = "Introduce el n\u00famero de categor\u00edas de la primera variable: "))
    ncolumnas <- as.numeric(readline(prompt = "Introduce el n\u00famero de categor\u00edas de la segunda variable: "))

    nombre_filas <- nombre_columnas <- c()

    # Introducir nombres de las filas
    for (j in 1:nfilas) {
      nombre_filas <- c(nombre_filas, readline(prompt = paste("Introduce el nombre de la fila n\u00famero ", j, ": ", sep = "")))
    }

    # Introducir nombres de las columnas
    for (k in 1:ncolumnas) {
      nombre_columnas <- c(nombre_columnas, readline(prompt = paste("Introduce el nombre de la columna n\u00famero ", k, ": ", sep = "")))
    }

    # Crear la matriz vacía
    x <- matrix(0, ncol = ncolumnas, nrow = nfilas)
    rownames(x) <- nombre_filas
    colnames(x) <- nombre_columnas

    # Permitir editar la matriz
    matriz_obs <- edit(x)
    print(matriz_obs)

  } else { # Caso cuando se proporciona un data frame

    # Verificar que el input es un data.frame
    if (!is.data.frame(x)) {
      stop("El input debe ser un data frame.")
    }

    # Mostrar los nombres de las columnas
    cat("Las columnas disponibles en el data frame son:\n")
    print(colnames(x))

    # Función auxiliar para seleccionar columnas por nombre o posición
    seleccionar_columna <- function(prompt_msg) {
      seleccion <- readline(prompt = prompt_msg)

      # Verificar si el usuario ingresó un número (posición) o un nombre (texto)
      if (suppressWarnings(!is.na(as.numeric(seleccion)))) {
        seleccion <- as.numeric(seleccion)
        if (seleccion < 1 || seleccion > ncol(x)) {
          stop("La posici\u00f3n seleccionada est\u00e1 fuera del rango de las columnas disponibles.")
        }
        return(seleccion)
      } else {
        # Selección por nombre
        if (!seleccion %in% colnames(x)) {
          stop("El nombre de columna introducido no existe.")
        }
        return(which(colnames(x) == seleccion))
      }
    }

    # Selección de las columnas
    col1 <- seleccionar_columna("Selecciona la primera variable (por nombre o posici\u00f3n): ")
    col2 <- seleccionar_columna("Selecciona la segunda variable (por nombre o posici\u00f3n): ")

    # Verificar que las variables seleccionadas son factores o caracteres
    if (!(is.factor(x[[col1]]) || is.character(x[[col1]])) ||
        !(is.factor(x[[col2]]) || is.character(x[[col2]]))) {
      stop("Ambas variables seleccionadas deben ser factores o caracteres.")
    }

    # Convertir a factor si son caracteres
    if (is.character(x[[col1]])) {
      x[[col1]] <- as.factor(x[[col1]])
      cat("La primera variable se ha convertido a factor.\n")
    }

    if (is.character(x[[col2]])) {
      x[[col2]] <- as.factor(x[[col2]])
      cat("La segunda variable se ha convertido a factor.\n")
    }

    # Extraer los datos de las variables seleccionadas
    data_frame_obs <- data.frame(x[[col1]], x[[col2]])
    colnames(data_frame_obs) <- c(colnames(x)[col1], colnames(x)[col2])

    # Mostrar los niveles de cada factor
    cat("Los niveles actuales de la primera variable son:\n")
    print(levels(data_frame_obs[[1]]))
    cat("Los niveles actuales de la segunda variable son:\n")
    print(levels(data_frame_obs[[2]]))

    # Preguntar al usuario si desea reordenar los niveles de la primera variable
    respuesta_reordenar_1 <- readline(prompt = "¿Quieres reordenar los niveles de la primera variable? \n 1. \"Si\" \n 2. \"No\" \n")

    if (respuesta_reordenar_1 == "1") {
      # Reordenar niveles de la primera variable
      cat("Reordena los niveles de la primera variable (separados por comas):\n")
      nuevo_orden1 <- unlist(strsplit(readline(), ","))
      levels(data_frame_obs[[1]]) <- nuevo_orden1
    }

    # Preguntar al usuario si desea reordenar los niveles de la segunda variable
    respuesta_reordenar_2 <- readline(prompt = "¿Quieres reordenar los niveles de la segunda variable? \n 1. \"Si\" \n 2. \"No\" \n")

    if (respuesta_reordenar_2 == "1") {
      # Reordenar niveles de la segunda variable
      cat("Reordena los niveles de la segunda variable (separados por comas):\n")
      nuevo_orden2 <- unlist(strsplit(readline(), ","))
      levels(data_frame_obs[[2]]) <- nuevo_orden2
    }

    # Crear la tabla de contingencia
    matriz_obs <- table(data_frame_obs[[1]], data_frame_obs[[2]])
    print(matriz_obs)

    # Confirmar la matriz observada
    respuesta <- readline(prompt = '¿Es esta la matriz de datos observados? \n 1. "Si" \n 2. "No" \n')

    if (respuesta == "2") {
      cat("Introduce o modifica la matriz de datos observados:\n")
      matriz_obs <- edit(matriz_obs)
      print(matriz_obs)

    } else if (respuesta == "1") {
      cat("¡Perfecto! Seguimos con estos datos.\n")
    } else {
      stop("El comando introducido no es correcto.")
    }
  }

  # Continuar con el análisis como antes...
  n <- sum(matriz_obs)
  mar_x <- apply(matriz_obs, 1, sum)
  mar_y <- apply(matriz_obs, 2, sum)
  matriz_esp <- crossprod(t(mar_x), mar_y) / n

  check_min_obs_extended_cols(matriz_obs, matriz_esp)

  g.l <- (nrow(matriz_obs) - 1) * (ncol(matriz_obs) - 1)

  if (g.l > 1) {
    estadistico.prueba <- sum(((matriz_obs - matriz_esp) ^ 2)  / matriz_esp)
  } else {
    estadistico.prueba <- sum(((abs(matriz_obs - matriz_esp) - 0.5) ^ 2)  / matriz_esp)
    warning('Se ha aplicado la correcci\u00f3n de Yates')
  }

  valor_critico <- qchisq(alfa, g.l, lower.tail = FALSE)

  if (estadistico.prueba < valor_critico) {
    print(paste("No se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ",
                round(estadistico.prueba, 2),
                ", se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n [0, ",
                round(valor_critico, 2), "]", sep=""))
  } else {
    print(paste("Se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ",
                round(estadistico.prueba, 2),
                ", se encuentra fuera de la regi\u00f3n de aceptaci\u00f3n [0, ",
                round(valor_critico, 2), "]", sep=""))
  }

  pvalor <- pchisq(estadistico.prueba, g.l, lower.tail = FALSE)

  H0 <- "Las variables son independientes"
  CH <- cbind(H0, estadistico.prueba, round(pvalor, 4))
  CH <- as.data.frame(CH)
  names(CH) <- c("Hipótesis nula", "estadístico de prueba", "p-valor")
  row.names(CH) <- NULL

  if (isTRUE(grafico)) {

    percentil99 <- qchisq(.9999, g.l)

    data <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
    data$y <- dchisq(data$x, g.l)

    name_plot <- ggplot(data, aes(x, y)) +
      geom_area(fill = "#C4961A") +
      geom_area(data = subset(data, x < valor_critico), fill = "#008B8B") +
      geom_vline(xintercept = 0, color = "black") +
      geom_vline(xintercept = estadistico.prueba, color = "#A52A2A", linetype = "dashed", size = 1) +
      labs(title = paste("Distribución Chi-Cuadrado con ", g.l, " grados de libertad", sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = round(c(0L, estadistico.prueba, valor_critico), 2)) +
      theme(axis.text.x = element_text(angle = 45))

    return(list(CH, name_plot))

  } else {
    return(CH)
  }
}
