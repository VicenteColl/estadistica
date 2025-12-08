#' Data: Encuesta cuatrienal de estructura salarial (2018)
#'
#' Datos del Instituto Nacional de Estadística. Los microdatos originales constan de 216,726 observaciones. A efectos de practicar, se han seleccionado aleatoriamente 1000 observaciones y 7 variables de interés.
#' @usage data("salarios2018")
#' @format Dataframe con 1000 observaciones 7 7 variables.
#' \describe{
#'   \item{SEXO}{Sexo (1=hombre, 6=mujer)}
#'   \item{ESTUDIOS}{Nivel de estudios. 1=Menos que primaria,2=Primaria,3=Primera etapa secundaria,4=Segunda etapa secundaria,5=FP superior o similar,6=Diplomado o similar,7=Licenciados o similares y doctores}
#'   \item{TIPO.JORNADA}{Tipo de jornada laboral. 1=Tiempo completo,2=Tiempo parcial}
#'   \item{TIPO.CONTRATO}{Tipo de contrato laboral. 1=Indefinido,2=Duración determinada}
#'   \item{SALARIO.BRUTO.ANUAL}{Salario bruto anual}
#'   \item{SALARIO.ORDINARIO.ANUAL}{Salario ordinario anual}
#'   \item{FACTOR.ELEVACION}{Factor de elevación}
#' }
#' @source Instituto Nacional de Estadística \url{https://www.ine.es/}
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
"salarios2018"

#' Data: Datos de empresas emergentes (startups)
#'
#' Datos simulados. Muestra de 21 empresas emergentes
#' @usage data("startup")
#' @format Dataframe con 21 observaciones de 4 variables.
#' \describe{
#'   \item{gasto.desarrollo}{Gastos de investigación y desarrollo, en euros.}
#'   \item{gasto.marketing}{Gastos de marketing, en euros.}
#'   \item{gasto.gestion}{Gastos de administración, en euros.}
#'   \item{beneficio}{Beneficios, en euros.}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
"startup"

#' Data: Ejemplo de dos variables (ejem_bidi)
#'
#' Datos simulados. Muestra de 100 observaciones
#' @usage data("ejem_bidi")
#' @format Dataframe con 100 observaciones de 2 variables.
#' \describe{
#'   \item{x}{Toma valores de 0 a 5.}
#'   \item{x}{Toma valores de 10 a 15}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
"ejem_bidi"

#' Datos simulados de dos muestras tomadas en periodos de tiempo distintos. La muestra 1 es tomada en enero y la muestra 2 en junio.
#' @usage data("diseno1")
#' @format Dataframe en formato ancho con 620 observaciones. La pregunta realizada es: ¿Sabe que Valencia es la capital mundial del diseño 2022?
#' \describe{
#'   \item{muestra1}{0: No sabe, 1: Sí Sabe}
#'   \item{muestra2}{0: No sabe, 1: Sí sabe}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
"diseno1"


#' Datos simulados de dos muestras tomadas en periodos de tiempo distintos. La muestra 1 es tomada en enero y la muestra 2 en junio.
#' @usage data("diseno2")
#' @format Dataframe en formato largo con 1085 observaciones. La pregunta realizada es: ¿Sabe que Valencia es la capital mundial del diseño 2022?
#' \describe{
#'   \item{muestra}{Toma dos valores: Muestra1 y Muestra2}
#'   \item{resultado}{0: No sabe, 1: Sí sabe}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano}.
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú}.
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo-García}.
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Facultad de Economía. Universidad de Valencia (España)
#'
"diseno2"

#' Data: Turistas internacionales Comunidad Valenciana
#'
#' @usage data("turistas")
#' @format Dataframe con 80 observaciones de 2 variables.
#' \describe{
#'   \item{perido}{Periodo temporal.}
#'   \item{Turistas.internacionales}{Número de turistas con destino principal la Comunidad Valenciana}
#' }
#' @source Movimientos turísticos en fronteras. Frontur. Instituto de Estudios Turísticos (hasta septiembre de 2015) e INE (a partir de octubre de 2015)
#'
"turistas2"

#' Data: Turistas por paises (WTO)
#'
#' Datos de World Tourism Organization.
#' @usage data("turistas")
#' @format Dataframe con 130 observaciones de 3 variables.
#' \describe{
#'   \item{País}{País de destino.}
#'   \item{Llegadas.turistas}{Número de llegada de turistas en 2017, en miles.}
#'   \item{Gasto.viajes}{Gasto en viajes en 2017, en millones de USD.}
#' }
#' @source World Tourism Organization (2019).
#'
"turistas"

#' Data: Hogares
#'
#' Datos de 10 hogares que se utilizan en los ejemplos de (1) tabla bidimensional, (2) covarianza, (3) matriz de covarianzas, (4) correlación y (5) matriz de correlación.
#' @usage data("hogares")
#' @format Dataframe con 10 observaciones de 3 variables.
#' \describe{
#'   \item{Hogares}{Identificación del hogar.}
#'   \item{ingresos}{Ingresos del hogar}
#'   \item{viajes}{Número de hogares realizado por los hogares.}
#' }
#'
"hogares"

#' Data: Viajes vendidos
#'
#' Datos de 5 observaciones que se utilizan en los ejemplos de (1) media, mediana y moda, (2) cuantiles, (3) varianza, desviación típica y coeficiente de variación, (4) medidas de forma y momento central y (5) resumen de descriptivos
#'
#' @usage data("viajes_vendidos")
#' @format Dataframe con 5 observaciones de 3 variables.
#' \describe{
#'   \item{Número.de.viajes.vendidos}{Número de viajes perdidos.}
#'   \item{Empleados}{Número de empleados}
#'   \item{Ni}{Frecuencia absoluta acumulada del número de empleados}
#' }
#'
"viajes_vendidos"
