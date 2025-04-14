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
#' @param alternativa Es un valor lógico. Si alternativa = TRUE el resultado de las medidas de forma muestra el coeficiente de asimetría y curtosis calculado según SPSS y EXCEL. Se facilita también los correspondientes errores típicos. Este argumento no funciona si pesos = NULL.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (exportar = TRUE).
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
#' El coeficiente de asimetría se obtiene a partir de la expresión:
#'
#' \if{html}{\figure{asimetriamuestra.png}{options: width="20\%" alt="Figure: asimetriamuestra.png"}}
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
#' \if{html}{\figure{asimetriapob.png}{options: width="20\%" alt="Figure: asimetriapob.png"}}
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
#' @seealso \code{\link{varianza}},\code{\link{desviacion}}
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

  if(is.numeric(x)){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)
  names(x) <- varnames

  if(is.null(variable)){

    varcuan <-  names(x)[which(sapply(x[varnames], is.numeric))]
    #seleccion = match(varcuan,varnames)
    x <- x[varcuan]
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
    varnames <- varnames[variable]
    names(x) <- varnames

  }

  if(!is.null(pesos) & !is.null(variable)){

    if((length(variable) | length(pesos)) > 1){

      stop("Para calcular la media a partir de la distribuci\u00fn de frecuencias solo puedes seleccionar una variable y unos pesos")

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

    if(pesos == variable){

      stop("Has seleccionado la misma columna del dataframe para la variable y los pesos")

    }


    x <- x[,c(variable,pesos)] %>% as.data.frame()
    varnames <- varnames[c(variable,pesos)]
    names(x) <- varnames

  }

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }


  if(is.null(pesos)){

    #N <- nrow(x)
    momento3 <- .momento.central(x,orden = 3)
    momento4 <- .momento.central(x,orden = 4)
    desv.x <- desviacion(x)

    asimetria <- momento3/desv.x^3
    curtosis <- momento4/desv.x^4 - 3

    forma <- bind_rows(asimetria,curtosis) %>%
      as.data.frame()
    row.names(forma) <- c("asimetria","curtosis")


  } else{

    desv.x <- desviacion(x,variable=1,pesos=2)
    forma <-  x %>%
        na.omit %>%
        rename(variable2 = varnames[1], pesos = varnames[2]) %>%
        mutate(media = media(x,variable=1,pesos=2),
                    sumatorio3 = (variable2-media)^3*pesos,
                    sumatorio4 = (variable2-media)^4*pesos) %>%
        summarize(momento3 = sum(sumatorio3)/sum(pesos),
                  momento4 = sum(sumatorio4)/sum(pesos),
                  asimetria = sum(sumatorio3)/(sum(pesos)*desv.x^3),
                  curtosis = sum(sumatorio4)/(sum(pesos)*desv.x^4) - 3)

    N <- sum(x[2])

    asimetria <- as.numeric(forma[3])
    curtosis <- as.numeric(forma[4])
    momento3 <- as.numeric(forma[1])
    momento4 <- as.numeric(forma[2])

    forma <- forma %>%
      select(asimetria,curtosis) %>%
      t() %>%
      as.data.frame()

    names(forma) <- varnames[1]
    row.names(forma) <- c("asimetria","curtosis")

  }

  if(isTRUE(alternativa) & is.null(pesos)){

      xalt <- x %>% gather(key="var_coef",value=value) %>%
        filter(complete.cases(.)) %>%
        group_by(var_coef) %>%
        summarize(N= n(),
                  c1 = (N*(N+1))/((N-1)*(N-2)*(N-3)),
                  c3 = (3*(N-1)^2)/((N-2)*(N-3)),
                  error_asimetria = sqrt((6*N*(N-1))/((N-2)*(N+1)*(N+3))),
                  error_curtosis = 2 * error_asimetria * sqrt((N^2-1)/((N-3)*(N+5)))
        ) %>% ungroup()

      desv.x.muestra = desviacion(x,tipo="cuasi")

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
      row.names(forma) <- varnames


  }

  # Exportar
  if (exportar) {

    filename <- paste0("Medidas_de_forma_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Medidas_de_forma")

    # nombres de fila a columna
    resumen_export <- cbind(forma = row.names(forma), forma)
    row.names(resumen_export) <- NULL

    openxlsx::writeData(wb, "Medidas_de_forma", resumen_export)

    # formato numerico decimal en Excel
    addStyle(wb, "Medidas_de_forma",
             style = createStyle(numFmt = "0.0000"),
             rows = 2:(nrow(resumen_export)+1),
             cols = 2:(ncol(resumen_export)+1),
             gridExpand = TRUE)

    saveWorkbook(wb, filename, overwrite = TRUE)
  }

  return(forma)

}
