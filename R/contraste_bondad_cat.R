#' @title Contraste de hipótesis de bondad de ajuste para datos categóricos.
#'
#' @description Contrasta si las probabilidades de que una observación pertenezca a cada una de las categorías de una variable categórica o criterio de clasificación se ajusta o no a unas probabilidades propuestas.
#'
#' Lee el código QR para video-tutorial 6666sobre el uso de la función con un ejemplo.
#'
#' @usage contraste_bondad_cat(x,
#'                             introducir = FALSE,
#'                             distribucion = "equiprobable",
#'                             alfa = 0.05,
#'                             grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe. En caso de haber más de una variable, el programa preguntará por la variable a seleccionar (por nombre o por posición) que debe ser un factor o carácter.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre el número de categorías de la variable, el nombre de cada categoría. A continuación se abrirá una ventana con un editor de datos y deberá introducir los valores de las frecuencias observadas.
#' @param distribucion Es un vector numérico. Deberá indicarse las probabilidades teóricas para cada categoría de la variable de la hipótesis nula. Por defecto, *distribucion="equiporobable"*.
#' @param alfa Es un valor numérico entre 0 y 1. Indica el nivel de significación. Por defecto, \code{alfa = 0.05} (5 por ciento)
#' @param grafico Es un valor lógico. Por defecto \code{grafico = FALSE}. Si se quiere obtener una representación gráfica del contraste realizado, cambiar el argumento a \code{grafico = TRUE}. Nota: Esta opción no está implementada para todos los casos.
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
#'(1) El estadístico del contraste de bondad de ajuste para datos categóricos es:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{k} \frac{(O_{i} - E_{i})^{2}}{E_{i}}}
#'
#' donde \eqn{O_{i}} son las frecuencias observadas y \eqn{E_{i}} son las frecuencias teóricas o esperadas, y se distribuye como:
#'
#' \deqn{\chi_{k-1}^{2}}
#'
#' donde \eqn{k} es el número de categorías de la variable.
#'
#'Además, se exige que todas las frecuencias teóricas no estén por debajo de 5. Si alguna no lo cumple es necesario reagrupar valores contiguos hasta conseguir superar esa cota.
#'
#' (2) Si el número de grados de libertad es 1, al estadístico del contraste se le aplica la siguiente corrección de Yates:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{k} \frac{(\left| O_{i} - E_{i} \right| - 0.5)^{2}}{E_{i}}}
#'
#'
#' @seealso \code{\link{contraste_bondad}}, \code{\link{contraste_independencia}},
#' \code{\link{contraste_homogeneidad}}
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
#' @importFrom stats dbinom dpois
#' @importFrom utils edit
#' @import dplyr ggplot2
#'
#' @export
contraste_bondad_cat <- function(x,
                             # variable = NULL,
                             introducir = FALSE,
                             distribucion = "equiprobable",
                             # parametro = FALSE,
                             alfa = 0.05,
                             grafico = FALSE) {


    # Si el usuario quiere introducir los datos manualmente
    if (introducir == TRUE) {

      # Caso cuando el usuario quiere introducir los datos manualmente
      nfilas <- as.numeric(readline(prompt = "Introduce el n\u00famero de categor\u00edas de la variable: "))

      nombre_filas <- c()

      # Introducir nombres de las filas
      for (j in 1:nfilas) {
        nombre_filas <- c(nombre_filas, readline(prompt = paste("Introduce el nombre de la categor\u00eda n\u00famero ", j, ": ", sep = "")))
      }

      # Crear la matriz vacia
      x <- matrix(0, ncol = 1, nrow = nfilas)
      rownames(x) <- nombre_filas
      colnames(x) <- "Frecuencias observadas"

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

      # Funcion auxiliar para seleccionar columna por nombre o posicion
      seleccionar_columna <- function(prompt_msg) {
        seleccion <- readline(prompt = prompt_msg)

        # Verificar si el usuario ingreso un numero (posicion) o un nombre (texto)
        if (suppressWarnings(!is.na(as.numeric(seleccion)))) {
          seleccion <- as.numeric(seleccion)
          if (seleccion < 1 || seleccion > ncol(x)) {
            stop("La posici\u00f3n seleccionada est\u00e1 fuera del rango de las columnas disponibles.")
          }
          return(seleccion)
        } else {
          # Seleccion por nombre
          if (!seleccion %in% colnames(x)) {
            stop("El nombre de columna introducido no existe.")
          }
          return(which(colnames(x) == seleccion))
        }
      }

      # Seleccionar la columna
      col <- seleccionar_columna("Selecciona la variable (por nombre o posici\u00f3n): ")

      # Verificar que la variable seleccionada es un factor o caracter
      if (!(is.factor(x[[col]]) || is.character(x[[col]]))) {
        stop("La variable seleccionada debe ser un factor o car\u00e1cter.")
      }

      # Convertir a factor si es un caracter
      if (is.character(x[[col]])) {
        x[[col]] <- as.factor(x[[col]])
        cat("La variable se ha convertido a factor.\n")
      }

      # Extraer los datos de la variable seleccionada
      data_frame_obs <- data.frame(x[[col]])
      colnames(data_frame_obs) <- colnames(x)[col]

      # Mostrar los niveles de la variable seleccionada
      cat("Los niveles actuales de la variable son:\n")
      print(levels(data_frame_obs[[1]]))

      # Preguntar al usuario si desea reordenar los niveles de la variable
      respuesta_reordenar <- readline(prompt = "¿Quieres reordenar los niveles de la variable? \n 1. \"Si\" \n 2. \"No\" \n")

      if (respuesta_reordenar == "1") {
        # Reordenar niveles de la variable
        cat("Reordena los niveles de la variable (separados por comas):\n")
        nuevo_orden <- unlist(strsplit(readline(), ","))
        levels(data_frame_obs[[1]]) <- nuevo_orden
      }

      # Crear la tabla de contingencia
      matriz_obs <- table(data_frame_obs[[1]])
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

    # Obtener las frecuencias observadas
    frecuencias_observadas <- as.numeric(matriz_obs)
    categorias <- length(frecuencias_observadas)

    # Verificar y calcular las frecuencias esperadas segun la distribucion
    if (is.character(distribucion) && distribucion == "equiprobable") {
      # Caso de distribucion equiprobable
      frecuencias_esperadas <- rep(sum(frecuencias_observadas) / categorias, categorias)
      cat("Se ha asumido una distribuci\u00f3n equiprobable.\n")

    } else if (is.numeric(distribucion)) {
      # Verificar que la longitud de la distribucion sea igual al numero de categorias
      if (length(distribucion) != categorias) {
        stop("El vector de distribuci\u00f3n debe tener la misma longitud que el n\u00famero de categor\u00edas.")
      }

      # Verificar que la suma de los valores de la distribucion sea 1 o 100% (con tolerancia)
      suma_distribucion <- sum(distribucion)
      if (!(abs(suma_distribucion - 1) < 1e-10 || abs(suma_distribucion - 100) < 1e-8)) {
        stop("La suma del vector de distribuci\u00f3n debe ser 1 o 100%.")
      }

      # Ajustar si la distribucion se ingreso en formato porcentual (con tolerancia)
      if (abs(suma_distribucion - 100) < 1e-8) {
        distribucion <- distribucion / 100
      }

      # Calcular las frecuencias esperadas en base a la distribucion proporcionada
      frecuencias_esperadas <- sum(frecuencias_observadas) * distribucion
      cat("Se ha utilizado la distribuci\u00f3n proporcionada por el usuario.\n")

    } else {
      stop("El argumento 'distribucion' debe ser 'equiprobable' o un vector num\u00e9rico.")
    }


    # Mostrar las frecuencias esperadas
    cat("Frecuencias esperadas:\n")
    print(frecuencias_esperadas)

    # Introducir las frecuencias esperadas como una nueva columna en la matriz
    matriz_completa <- cbind(Freq_obs = frecuencias_observadas, Freq_esp = frecuencias_esperadas)
    matriz_completa <- as.data.frame(matriz_completa)
    rownames(matriz_completa) <- row.names(matriz_obs)

    # Mostrar la matriz completa con frecuencias observadas y esperadas
    cat("Matriz con frecuencias observadas y esperadas:\n")
    print(matriz_completa)

  if(sum(matriz_completa$Freq_esp < 5) > 0){
    message("Aquí tienes la tabla recalcudada de frecuencias esperadas porque alguna de las frecuecias te\u00f3ricas era menor a 5. Para llevar a cabo el test es necesario reagrupar las categorías.")
    matriz_completa <- .check_min_obs(matriz_completa)
    print(matriz_completa)
  }

  n <- length(matriz_completa$Freq_obs)

  g.l <- n - 1

  if(g.l == 1){

    warning("Los grados de libertad son 1; por tanto, es necesario aplicar la correcci\u00f3n de Yates.")
    estadistico.prueba <- sum((abs(matriz_completa$Freq_obs - matriz_completa$Freq_esp)-0.5)^2/matriz_completa$Freq_esp)

  }else{

    estadistico.prueba <- sum((matriz_completa$Freq_obs - matriz_completa$Freq_esp)^2/ matriz_completa$Freq_esp)

  }

  valor_critico <- qchisq(alfa, g.l, lower.tail = F)


  if(estadistico.prueba < valor_critico ){

    print(paste("No se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

  } else{

    print(paste("Se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra fuera de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

  }

  pvalor <- pchisq(estadistico.prueba, g.l, lower.tail = F)

  H0 <- paste("Los datos siguen una distribuci\u00f3n ",  paste(distribucion, collapse = ", "))
  CH <- cbind(H0, estadistico.prueba, round(pvalor, 4))
  CH <- as.data.frame(CH)
  names(CH) <- c("Hip\u00f3tesis nula", "estad\u00edstico de prueba", "p-valor")
  row.names(CH) <- NULL

  if(isTRUE(grafico)){

    percentil99 <- qchisq(.9999, g.l)

    data <- data.frame(x=seq(from = 0, to = percentil99, percentil99/200))
    data$y <-dchisq(data$x, g.l)

    name_plot <- ggplot(data, aes(x, y)) +
      geom_area(fill = "#C4961A") +
      geom_area(data = subset(data, x < valor_critico), fill = "#008B8B") +
      geom_vline(xintercept = 0, color = "black") +
      geom_vline(xintercept = estadistico.prueba, color = "#A52A2A", linetype = "dashed" , size = 1) +
      labs(title = paste("Distribuci\u00f3n Chi-Cuadrado con ", g.l," grados de libertad", sep=""), x = "", y = "") +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(breaks = round(c(0L, estadistico.prueba, valor_critico), 2)) +
      theme(axis.text.x = element_text(angle = 45))

    return(list(CH, name_plot))

  } else{

    return(CH)

  }

} # Fin de la funcion


