#' @title Contraste de Hipótesis de Bondad de Ajuste.
#'
#' @description Contrasta si los datos de una muestra proceden de una distribución poblacional determinada.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrcmedia.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrcmedia.png}{options: width=3cm}}

#' @usage contraste_bondad(x,
#'                  variable = NULL,
#'                  introducir = FALSE,
#'                  distribucion = "equiprobable",
#'                  parametro = FALSE,
#'                  alfa = 0.05,
#'                  grafico = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante sobre el número de filas (se abrirá una ventana con un editor de datos y deberá introducir los valores de la variable poblacional y las frecuencias observadas), valor del parámetro poblacional, etc.
#' @param distribucion Es un carácter. Indica el tipo de distribución poblacional que se quiere contrastar en la hipótesis nula (por defecto, \code{distribucion = "equiprobable"}) o desconocida. En este último caso debería cambiarse el argumento a \code{var_pob = "desconocida"}.
#'        Si \code{distribucion = "equiprobable"}, se contrasta que en la distribución poblacional de la hipótesis nula todos los valores de la población tienen la misma probabilidad.
#'        Si \code{distribucion = "poisson"}, se contrasta que la distribución poblacional de la hipótesis nula se distribuye según una Poisson.
#'        Si \code{distribucion = "binomial"}, se contrasta que la distribución poblacional de la hipótesis nula se distribuye según una Binomial.
#' @param parametro Es un valor lógico. Si no se especifica ningún valor para el parámetro o parámetros de la distribución poblacional \code{parametro = FALSE} (por defecto) o si se especifica un valor para dicho parámetro o parámetros (cambiar el argumento a \code{parametro = TRUE})
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
#' \strong{Josep Lledó Benito}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' \strong{Francisco Palmi Perales}.
#' \emph{Estadística e Investigación Operativa.}
#'
#' Facultad de Matemáticas. Universidad de Valencia (España)
#'
#' @details
#'
#' (1) El estadístico del contraste de bondad de ajuste es:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{k} \frac{(O_{i} - E_{i})^{2}}{E_{i}}}
#'
#' donde \(O_{i}\) son las frecuencias observadas y \(E_{i}\) son las frecuencias teóricas o esperadas.
#'
#' \deqn{\chi_{k-m-1}^{2}}
#'
#' donde \(k\) es el número de valores distintos de la variable, y \(m\) es el número de parámetros de la distribución poblacional de la hipótesis nula no especificados (desconocidos) y que se han tenido que estimar.
#'
#' Además, se exige que todas las frecuencias teóricas no estén por debajo de 5. Si alguna no lo cumple, es necesario reagrupar valores contiguos hasta conseguir superar esa cota.
#'
#' (2) Si el número de grados de libertad es 1, al estadístico del contraste se le aplica la siguiente corrección de Yates:
#'
#' \deqn{\chi ^{2} = \sum_{i=1}^{k} \frac{(\left| O_{i} - E_{i} \right| - 0.5)^{2}}{E_{i}}}
#'
#' @seealso \code{\link{contraste_independencia}},\code{\link{contraste_homogeneidad}}
#'
#' @references
#' Casas José M. (1997) Inferencia estadística. Editorial: Centro de estudios Ramón Areces, S.A. ISBN: 848004263-X
#'
#' Esteban García, J. et al. (2008). Curso básico de inferencia estadística. ReproExprés, SL. ISBN: 8493036595.
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034

  #' @importFrom stats dbinom dpois
  #' @importFrom utils edit
  #' @import dplyr ggplot2
  #'
  #' @export

contraste_bondad <- function(x,
                             variable = NULL,
                             introducir = FALSE,
                             distribucion = "equiprobable",
                             parametro = FALSE,
                             alfa = 0.05,
                             grafico = FALSE){


  if(isFALSE(introducir)) {

    if(is.numeric(x)){

      matriz <- data.frame("Valores" = unique(x),
                           "Freq_obs" = as.numeric(table(x)));print(matriz)

      respuesta <- readline(prompt ='¿Estas son las frecuencias observadas? \n 1. "Si" \n 2. "No" \n')

      if (respuesta == "2"){
        print("Has marcado que estas no son las frecuencias, introduce o modifica lo que creas necesario.")
        matriz <- edit(matriz)
        matriz

      } else if (respuesta == "1"){
        print("¡Perfecto! Seguimos con estos datos")

      } else {
        stop("El comando introducido no es correcto")

      } # End if respuesta

    } else {

      stop("x debe ser un vector de números")

    } # Fin de is.numeric

  } else {
    # se introducen los datos manualmente
    nfilas <- as.numeric(readline(prompt = "Intoduce el número de filas: "))

    x <- matrix(0, nrow = nfilas, ncol=2)

    matriz <- edit(x)
    matriz <- as.data.frame(matriz); matriz
    colnames(matriz) <- c("Valores", "Freq_obs")

  } # Fin de introducir datos

  if (distribucion == "equiprobable"){

    # Equiprobable

    n <- length(matriz$Freq_obs)
    matriz$Freq_esp <-  sum(matriz$Freq_obs) / length(matriz$Freq_obs)

  } else if (distribucion == "poisson"){

    # poisson

    if (parametro == F) {
      n <- length(matriz$Freq_obs) - 1
      lambda <- as.numeric(crossprod(matriz$Valores, matriz$Freq_obs)) / sum(matriz$Freq_obs)
    } else{
      n <- length(matriz$Freq_obs)
      lambda <- as.numeric(readline(prompt = "Intoduce el valor de lambda: "))
    } # End if

    matriz$Freq_esp <-dpois(matriz$Valores,lambda) * sum(matriz$Freq_obs)

  } else if (distribucion == "binomial"){

    # Binomial

    if (parametro == F) {
      n <- length(matriz$Freq_obs) - 1
      media <- as.numeric(crossprod(matriz$Valores, matriz$Freq_obs)) / sum(matriz$Freq_obs)
      p <- media / n
    } else{
      n <- length(matriz$Freq_obs)
      #n <- as.numeric(readline(prompt = "Intoduce el valor de n: "))
      p <- as.numeric(readline(prompt = "Intoduce el valor de p: "))
    } # End if

    matriz$Freq_esp <- dbinom(matriz$Valores, n, p ) * sum(matriz$Freq_obs)

  } else {

    stop("No has introducido una distribución de probabilidad disponible")

  } # Fin de distribucion

  # estimacion de parametros de binonial y poisson
  if (distribucion == "equiprobable"){

    # Equiprobable

    k <- 0

  } else if (distribucion == "poisson"){

    # poisson

    if (parametro == F) {
      k <- 1
    } else{
      k <- 0
    } # End if

  } else if (distribucion == "binomial"){

    # Binomial

    if (parametro == F) {
      k <- 1
    } else{
      k <- 0
    } # End if

  } else {

    stop("No has introducido una distribución de probabilidad disponible")

  } # Fin de estimacion de parametros de distribucion

  # reagrupar si frecuencias esperadas es menor a 5
  if(sum(matriz$Freq_esp < 5) > 0){
    message("Aquí tienes la tabla recalcudada de frecuencias esperadas porque alguna de las frecuecias teóricas era menor a 5. Para llevar a cabo el test es necesario reagrupar las categorías.")
    matriz <- check_min_obs(matriz)
    print(matriz)
  }

  # Sumar las frecuencias observadas
  suma_freq_obs <- sum(matriz$Freq_obs)

  # Comprobar si las frecuencias esperadas suman la suma de las frecuencias observadas
  if (sum(matriz$Freq_esp) != suma_freq_obs) {
    diferencia <- suma_freq_obs - sum(matriz$Freq_esp)

    # Ajustar la última categoría
    matriz$Freq_esp[nrow(matriz)] <- matriz$Freq_esp[nrow(matriz)] + diferencia

    # Opcional: advertir al usuario del ajuste
    message("Se ha ajustado la última categoría en ", diferencia, " para que las frecuencias esperadas sumen ", suma_freq_obs, ".")
  }


  n <- length(matriz$Freq_obs)

  g.l <- n - k - 1

  if(g.l == 1){

    warning("Los grados de libertad son 1; por tanto, es necesario aplicar la corrección de Yates.")
    estadistico.prueba <- sum((abs(matriz$Freq_obs-matriz$Freq_esp)-0.5)^2/matriz$Freq_esp)

  }else{

    estadistico.prueba <- sum((matriz$Freq_obs - matriz$Freq_esp)^2/ matriz$Freq_esp)

  }

  valor_critico <- qchisq(alfa, g.l, lower.tail = F)


  if(estadistico.prueba < valor_critico ){

    print(paste("No se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra dentro de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

  } else{

    print(paste("Se rechaza la hip\u00f3tesis nula. El valor del estad\u00edstico de contraste: ", round(estadistico.prueba, 2) , ", se encuentra fuera de la regi\u00f3n de aceptaci\u00f3n que toma el intervalo [0, ", round(valor_critico, 2), "]", sep=""))

  }

  pvalor <- pchisq(estadistico.prueba, g.l, lower.tail = F)

  H0 <- paste("Los datos siguen una distribución ", distribucion)
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


