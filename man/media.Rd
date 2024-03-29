% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/media.R
\name{media}
\alias{media}
\title{Media (aritmética).}
\usage{
media(x, variable = NULL, pesos = NULL)
}
\arguments{
\item{x}{Conjunto de datos. Puede ser un vector o un dataframe.}

\item{variable}{Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.}

\item{pesos}{Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.}
}
\value{
Si \code{pesos = NULL}, devuelve la media (aritmética) de todas la variables seleccionadas en un \code{vector}. En caso contrario, devuelve únicamente la media de la variable para la que se ha facilitado la distribución de frecuencias.
}
\description{
Calcula la media aritmética.

Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.

\if{html}{\figure{qrposicion.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
\if{latex}{\figure{qrposicion.png}{options: width=3cm}}
}
\details{
Si se obtiene la media (muestral) a partir de los datos brutos, como generalmente hacen los softwares:

\if{html}{\figure{media.png}{options: width="80" alt="Figure: media.png"}}
\if{latex}{\figure{media.png}{options: width=3.5cm}}

Si se desea obtener la media (muestral) a partir de una tabla estadística se utiliza la expresión:

\if{html}{\figure{media2.png}{options: width="80" alt="Figure: media2.png"}}
\if{latex}{\figure{media2.png}{options: width=3.5cm}}
}
\note{
Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene la media poblacional:

\if{html}{\figure{mediapob.png}{options: width="80" alt="Figure: mediapob.png"}}
\if{latex}{\figure{mediapob.png}{options: width=3cm}}
}
\examples{

media1 <- media(startup[1])
media2 <- media(startup,variable=1)
media3 <- media(salarios2018,variable=6,pesos=7)

}
\references{
Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741

Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034

Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
}
\author{
\strong{Vicente Coll-Serrano}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

\strong{Rosario Martínez Verdú}.
\emph{Economía Aplicada.}

\strong{Cristina Pardo-García}.
\emph{Métodos Cuantitativos para la Medición de la Cultura (MC2). Economía Aplicada.}

Facultad de Economía. Universidad de Valencia (España)
}
