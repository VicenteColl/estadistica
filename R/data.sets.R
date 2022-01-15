#' Data: Encuesta cuatrienal de estructura salarial (2018)
#'
#' Datos del Instituto Nacional de Estadística. Hay un total de 216,726 observaciones de 10 variables seleccionadas. Los datos han sido tratados siguiendo las instrucciones que el INE adjunta con los microdatos.
#' @usage data("salarios2018")
#' @format Dataframe con 216726 observaciones de 10 variables.
#' \describe{
#'   \item{ID}{Identificador del encuestado.}
#'   \item{CODIGO.REGION}{1=NOROESTE,2=NORESTE,3=COMUNIDAD MADRID,4=CENTRO,5=ESTE,6=SUR,7=CANARIAS}
#'   \item{SEXO}{Sexo (1=hombre, 6=mujer)}
#'   \item{ESTUDIOS}{Nivel de estudios. 1=Menos que primaria,2=Primaria,3=Primera etapa secundaria,4=Segunda etapa secundaria,5=FP superior o similar,6=Diplomado o similar,7=Licenciados o similares y doctores}
#'   \item{TIPO.JORNADA}{Tipo de jornada laboral. 1=Tiempo completo,2=Tiempo parcial}
#'   \item{TIPO.CONTRATO}{Tipo de contrato laboral. 1=Indefinido,2=Duración determinada}
#'   \item{SALARIO.BRUTO.ANUAL}{Salario bruto anual}
#'   \item{SALARIO.ORDINARIO.ANUAL}{Salario ordinario anual}
#'   \item{PAGOS.EXTRA}{Pagos extraordinarios anuales}
#'   \item{FACTOR.ELEVACION}{Factor de elevación}
#' }
#' @source Instituto Nacional de Estadística \url{http://www.ine.es/}
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

#' Datos simulados sobre características físicas de bebés.
#' @usage data("crecimiento")
#' @format Dataframe con 4800 observaciones de 7 variables.
#' \describe{
#'   \item{Id.bebe}{identificador del bebé}
#'   \item{edad}{edad del bebé, en meses}
#'   \item{sexo}{sexo del bebé}
#'   \item{estatura}{estatura del bebé, en centímetros}
#'   \item{peso}{peso del bebé, en kilogramos}
#'   \item{perimetro.craneal}{perímetro del cráneo del bebé, en centímetros}
#'   \item{IMC}{índice de masa corporal, en kg/m2. Es la relación entre el peso (en kg) y la longitud (estatura) en posición recostada (en m2)}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Universidad de Valencia (España)
#'
"crecimiento"

#' Datos simulados del rendimiento de dos acciones.
#' @usage data("rendimientos")
#' @format Dataframe con 3000 observaciones de 3 variables.
#' \describe{
#'   \item{Dia}{dia}
#'   \item{X}{rendimiento mensual de la acción X, en euros}
#'   \item{Y}{rendimiento mensual de la acción Y, en euros}
#' }
#' @source Muestra simulada.
#'
#' @author
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#'
#' \strong{Rosario Martínez Verdú} (\email{rosario.martinez@@uv.es}).
#' \emph{Economía Aplicada.}
#'
#' \strong{Cristina Pardo García} (\email{cristina.pardo-garcia@@uv.es}).
#' \emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}
#'
#' Universidad de Valencia (España)
#'
"rendimientos"

#' Data: turistas
#'
#' Datos del Instituto Nacional de Estadística. Hay un total de 216,726 observaciones de 10 variables seleccionadas. Los datos han sido tratados siguiendo las instrucciones que el INE adjunta con los microdatos.
#' @usage data("turistas")
#' @format Dataframe con 80 observaciones de 2 variables.
#' \describe{
#'   \item{perido}{Periodo temporal.}
#'   \item{Turistas.internacionales}{Número de turistas internacionales}
#' }
#' @source Instituto Nacional de Estadística \url{http://www.ine.es/}
#'
"turistas"


