#' @title Medidas de forma
#'
#' @description Calcula el coeficiente de asimetría y de curtosis de Fisher.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrforma.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrforma.png}{options: width=3cm}}
#'
#' @usage medidas.forma(x,
#' variable = NULL,
#' pesos = NULL,
#' alternativa = FALSE,
#' exportar = FALSE)
#'
#' @param x Conjunto de datos, que puede estar formado por una o más variables.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de x. Si x se refiere una sola variable, el argumento variable es NULL. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param alternativa Es un valor lógico. Si alternativa = TRUE el resultado de las medidas de forma muestra el coeficiente de asimetría y curtosis calculado según SPSS y EXCEL. Se facilita también los correspondientes errores típicos.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
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
#' El coeficiente de asimetría se obtiene a partir de la expresión:
#'
#' \if{html}{\figure{asimetriamuestra.png}{options: width="25\%" alt="Figure: asimetriamuestra.png"}}
#' \if{latex}{\figure{asimetriamuestra.png}{options: width=3cm}}
#'
#' y el coeficiente de curtosis:
#'
#' \if{html}{\figure{curtosismuestra.png}{options: width="35\%" alt="Figure: curtosismuestra.png"}}
#' \if{latex}{\figure{curtosismuestra.png}{options: width=4cm}}
#'
#' @note
#' (1) El coeficiente de asimetría poblacional es:
#'
#' \if{html}{\figure{asimetriapob.png}{options: width="25\%" alt="Figure: asimetriapob.png"}}
#' \if{latex}{\figure{asimetriapob.png}{options: width=3cm}}
#'
#' (2) El coeficiente de curtosis poblacional es:
#'
#' \if{html}{\figure{curtosispob.png}{options: width="35\%" alt="Figure: curtosispob.png"}}
#' \if{latex}{\figure{curtosispob.png}{options: width=4cm}}
#'
#' (3) Si el argumento alternativa = TRUE, se obtienen los resultados de asimetría y curtosis que generalmente ofrecen softwares como: SPSS, Stata, SAS, Excel, etc.
#'
#'
#' \if{html}{\figure{asimetriasoft.png}{options: width="60\%" alt="Figure: asimetriasoft.png"}}
#' \if{latex}{\figure{asimetriasoft.png}{options: width=8cm}}
#'
#'  \if{html}{\figure{curtosissoft.png}{options: width="120\%" alt="Figure: curtosissoft.png"}}
#' \if{latex}{\figure{curtosissoft.png}{options: width=13cm}}
#'
#' @seealso \code{\link{momento.central}},\code{\link{varianza}},\code{\link{desviacion}}
#'
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @importFrom stats complete.cases
#' @examples
#'
#' forma <- medidas.forma(startup)
#' forma2 <- medidas.forma(startup, alternativa= TRUE)
#'
#' @export
medidas.forma <- function(x,
                          variable = NULL,
                          pesos = NULL,
                          alternativa = FALSE,
                          exportar = FALSE){

  x <- data.frame(x)
  x <- x[,order(names(x))]
  varnames <- names(x)

  if(is.null(variable)){

    varcuan <-  names(x[unlist(lapply(x, is.numeric))])
    seleccion = match(varcuan,varnames)
    x <- x[seleccion]
    varnames <- varcuan

  } else{

    if(is.numeric(variable)){

      if(all(variable <= length(x))){

        variable <- variable


      } else{

        stop("Selecci\u00f3n err\u00f3nea de variables")

      }
    }

    if(is.character(variable)){

      if(all(variable %in% varnames)){
        variable = match(variable,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }

  }


  if(is.null(pesos) & !is.null(variable)){

    x <- x[,variable] %>% as.data.frame()
    x <- x[,order(names(x))]
    varnames <- names(x)

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular las medidas de forma a partir de la distribuci\u00f3n de frecuencias solo puedes seleccionar una variable y unos pesos")

    }

    if(is.numeric(pesos)){

      pesos <- pesos

    }


    if(is.character(pesos)){

      if(pesos %in% varnames){
        pesos = match(pesos,varnames)
      } else {
        stop("El nombre de los pesos no es v\u00e1lido")
      }
    }


    x <- x[,c(variable,pesos)] %>%
      na.omit(x) %>%
      as.data.frame()
    varnames <- varnames[c(variable,pesos)]

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }


  if(is.null(pesos)){

    #N <- nrow(x)
    momento3 <- apply(x,2,momento.central,orden = 3)
    momento4 <- apply(x,2,momento.central,orden = 4)
    desv.x <- as.numeric(desviacion(x))

    asimetria <- momento3/desv.x^3
    curtosis <- momento4/desv.x^4


  } else{

    desv.x <- as.numeric(desviacion(x,variable=1,pesos=2))
    forma <-  x %>%
        na.omit %>%
        rename(variable2 = varnames[1], pesos = varnames[2]) %>%
        mutate(media = as.numeric(media(x,variable=1,pesos=2)),
                    sumatorio3 = (variable2-media)^3*pesos,
                    sumatorio4 = (variable2-media)^4*pesos) %>%
        summarize(momento3 = sum(sumatorio3)/sum(pesos),
                  momento4 = sum(sumatorio4)/sum(pesos),
                  asimetria = sum(sumatorio3)/(sum(pesos)*desv.x^3),
                  curtosis = sum(sumatorio4)/(sum(pesos)*desv.x^4))

    N <- sum(x[2])

    asimetria <- as.numeric(forma[3])
    curtosis <- as.numeric(forma[4])
    momento3 <- as.numeric(forma[1])
    momento4 <- as.numeric(forma[2])

    varnames <- varnames[1]

  }

  if(alternativa == TRUE){


    xalt <- x %>% gather(key="var_coef",value=value) %>%
      filter(complete.cases(.)) %>%
      group_by(var_coef) %>%
      summarize(N= n(),
                c1 = (N*(N+1))/((N-1)*(N-2)*(N-3)),
                c3 = (3*(N-1)^2)/((N-2)*(N-3)),
                error_asimetria = sqrt((6*N*(N-1))/((N-2)*(N+1)*(N+3))),
                error_curtosis = 2 * error_asimetria * sqrt((N^2-1)/((N-3)*(N+5)))
                ) %>% ungroup()

    desv.x.muestra = as.numeric(desviacion(x,tipo="cuasi"))
    momento4 <- as.numeric(momento4)
    momento3 <- as.numeric(momento3)

    xalt <- xalt %>%
      mutate(desv.x.muestra = desv.x.muestra,
             c2 = (N*momento4)/desv.x.muestra^4,
             curtosis_soft = (c1*c2)-c3,
             A1 = N/((N-1)*(N-2)),
             A2 = (N*momento3)/desv.x.muestra^3,
             asimetria_soft = A1*A2) %>%
      ungroup()

    forma <- xalt %>%
      select(2,5,6,9,12) %>%
      mutate(asimetria = asimetria,
             curtosis = curtosis) %>%
      select(N="N",asimetria="asimetria",curtosis="curtosis",asimetria2="asimetria_soft",
             error_asimetria2="error_asimetria",
             curtosis2="curtosis_soft",error_curtosis2="error_curtosis") %>%
      as.data.frame()


  } else{

    forma <- data.frame(asimetria=asimetria,curtosis=curtosis)

  }

  row.names(forma) <- varnames

  if (exportar) {
    filename <- paste("Medidas de forma"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)
    rio::export(forma, row.names = TRUE, file = filename)
  }

  return(forma)

}
