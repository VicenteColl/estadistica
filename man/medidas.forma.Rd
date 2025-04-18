% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/medidas.forma.R
\name{medidas.forma}
\alias{medidas.forma}
\title{Medidas de forma}
\usage{
medidas.forma(x,
variable = NULL,
pesos = NULL,
alternativa = FALSE,
exportar = FALSE)
}
\arguments{
\item{x}{Conjunto de datos, que puede estar formado por una o más variables.}

\item{variable}{Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.}

\item{pesos}{Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.}

\item{alternativa}{Es un valor lógico. Si alternativa = TRUE el resultado de las medidas de forma muestra el coeficiente de asimetría y curtosis calculado según SPSS y EXCEL. Se facilita también los correspondientes errores típicos. Este argumento no funciona si pesos = NULL.}

\item{exportar}{Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).}
}
\description{
Calcula el coeficiente de asimetría y de curtosis de Fisher.

Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.

\if{html}{\figure{qrforma.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
\if{latex}{\figure{qrforma.png}{options: width=3cm}}
}
\details{
El coeficiente de asimetría se obtiene a partir de la expresión:

\if{html}{\figure{asimetriamuestra.png}{options: width="20\%" alt="Figure: asimetriamuestra.png"}}
\if{latex}{\figure{asimetriamuestra.png}{options: width=3cm}}

y el coeficiente de curtosis:

\if{html}{\figure{curtosismuestra.png}{options: width="35\%" alt="Figure: curtosismuestra.png"}}
\if{latex}{\figure{curtosismuestra.png}{options: width=4cm}}
}
\note{
(1) El coeficiente de asimetría poblacional es:

\if{html}{\figure{asimetriapob.png}{options: width="20\%" alt="Figure: asimetriapob.png"}}
\if{latex}{\figure{asimetriapob.png}{options: width=3cm}}

(2) El coeficiente de curtosis poblacional es:

\if{html}{\figure{curtosispob.png}{options: width="35\%" alt="Figure: curtosispob.png"}}
\if{latex}{\figure{curtosispob.png}{options: width=4cm}}

(3) Si el argumento alternativa = TRUE, se obtienen los resultados de asimetría y curtosis que generalmente ofrecen softwares como: SPSS, Stata, SAS, Excel, etc.


\if{html}{\figure{asimetriasoft.png}{options: width="60\%" alt="Figure: asimetriasoft.png"}}
\if{latex}{\figure{asimetriasoft.png}{options: width=8cm}}

 \if{html}{\figure{curtosissoft.png}{options: width="120\%" alt="Figure: curtosissoft.png"}}
\if{latex}{\figure{curtosissoft.png}{options: width=13cm}}
}
\examples{

forma <- medidas.forma(startup)
forma2 <- medidas.forma(startup, alternativa= TRUE)

}
\references{
Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741

Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034

Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
}
\seealso{
\code{\link{varianza}},\code{\link{desviacion}}
}
\author{
\strong{Vicente Coll-Serrano}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

\strong{Rosario Martínez Verdú}.
\emph{Economía Aplicada.}

Facultad de Economía. Universidad de Valencia (España)
}
