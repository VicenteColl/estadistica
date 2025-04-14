#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdescriptivos.png}{options: width="25\%" alt="Figure: qricvarianza.png"}}
#' \if{latex}{\figure{qrdescriptivos.png}{options: width=3cm}}
#'
#' @usage resumen.descriptivos(x,
#'                             variable = NULL,
#'                             pesos = NULL,
#'                             exportar = FALSE)
#'
#' @param x Conjunto de datos. Puede ser un vector o un dataframe.
#' @param variable Es un vector (numérico o carácter) que indica las variables a seleccionar de \code{x}. Si \code{x} se refiere una sola variable, \code{variable = NULL}. En caso contrario, es necesario indicar el nombre o posición (número de columna) de la variable.
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias, debe indicarse la columna que representa los valores de la variable y la columna con las frecuencias o pesos.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Esta función devuelve los principales estadísticos descriptivos muestrales en un objeto de tipo \code{data.frame}. Los descriptivos que se obtienen son: media, mínimo, cuartil 1, mediana, cuartil 3, máximo, varianza muestral, desviación típica muestral, coeficiente de variación, recorrido inter-cuartílico, asimetría, curtosis y moda.
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
#' @references
#' Esteban García, J. y otros. (2005). Estadística descriptiva y nociones de probabilidad. Paraninfo. ISBN: 9788497323741
#'
#' Newbold, P, Carlson, W. y Thorne, B. (2019). Statistics for Business and Economics, Global Edition. Pearson. ISBN: 9781292315034
#'
#' Murgui, J.S. y otros. (2002). Ejercicios de estadística Economía y Ciencias sociales. tirant lo blanch. ISBN: 9788484424673
#'
#' @examples
#'
#' descriptivos <- resumen.descriptivos(startup)
#'
#' @import dplyr openxlsx
#'
#' @export
resumen.descriptivos <- function(x,
                                 variable = NULL,
                                 pesos = NULL,
                                 exportar = FALSE){

  # 1. Guardar TODAS las opciones originales
  old_options <- options()

  # 2. Configurar para evitar notación científica
  options(scipen = 999,  # Valor muy alto para forzar decimales
          digits = 15)    # Número de dígitos a mostrar

  # 3. Asegurar que se restauren las opciones al salir
  on.exit(options(old_options), add = TRUE)

  # Capturar el nombre original si es un vector
  var_name <- deparse(substitute(x))

  # Manejo de nombres para diferentes tipos de entrada
  if (is.data.frame(x) || is.list(x)) {
    # Para data.frames/listas
    original_names <- names(x)
    x <- as.data.frame(x)

    if (is.null(variable)) {
      varnames <- names(x)[sapply(x, is.numeric)]
    } else {
      if (is.numeric(variable)) {
        varnames <- names(x)[variable]
      } else {
        varnames <- variable
      }
    }
  } else {
    # Para vectores
    if (grepl("\\$", var_name)) {
      # Si es de tipo dataframe$columna
      varnames <- sub(".*\\$", "", var_name)
    } else {
      # Si es un vector simple
      varnames <- "variable"
    }
    x <- data.frame(x)
    names(x) <- varnames
    original_names <- varnames
  }

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

    #names(x) <- varnames
    valor_media <- media(x) %>% t() %>% as.data.frame()
    valor_cuartiles <- cuantiles(x, cortes = c(0,0.25,0.5,0.75,1))
    valor_varianza <- varianza(x) %>% t() %>% as.data.frame()
    valor_desviacion <- desviacion(x) %>% t() %>% as.data.frame()
    valor_coef_variacion <- coeficiente.variacion(x)%>% t() %>% as.data.frame()
    ric <- cuantiles(x, cortes = 0.75) - cuantiles(x, cortes = 0.25)
    valor_forma <- medidas.forma(x) %>% as.data.frame
    valor_moda <- moda(x)


  } else{

    valor_media <- media(x,variable=1,pesos=2) %>% as.data.frame()
    #names(valor_media)
    valor_cuartiles <- cuantiles(x,variable=1,pesos=2, cortes = c(0,0.25,0.5,0.75,1))
    valor_varianza <- varianza(x,variable=1,pesos=2) %>%  as.data.frame()
    valor_desviacion <- desviacion(x,variable=1,pesos=2) %>% as.data.frame()
    valor_coef_variacion <- coeficiente.variacion(x,variable=1,pesos=2) %>% as.data.frame()
    ric <- cuantiles(x,variable=1,pesos=2, cortes = 0.75) - cuantiles(x,variable=1,pesos=2, cortes = 0.25)
    valor_forma <- medidas.forma(x,variable=1,pesos=2)
    valor_moda <-moda(x,variable=1,pesos=2)

    varnames <- varnames[1]

  }

  resumen <- data.table::rbindlist(list(valor_media,
                                        valor_cuartiles,
                                        valor_varianza,
                                        valor_desviacion,
                                        valor_coef_variacion,
                                        ric,
                                        valor_forma,
                                        valor_moda),
                                   use.names = FALSE)

  resumen <- as.data.frame(resumen) %>%
    round(4)
  names(resumen) <- varnames

  num_modas <- nrow(valor_moda)
  row.names(resumen) <- c("media","m\u00ednimo","cuartil 1","mediana","cuartil 3", "m\u00e1ximo","varianza","desviaci\u00f3n t\u00edpica",
                            "coef.variaci\u00f3n","RIC","asimetr\u00eda","curtosis",paste("moda_",1:num_modas,sep=""))

  # Mostrar resultados (viene de utils.R)
  .mostrar_lista_resultados(resumen, "Resumen de estadísticos descriptivos")

  # Exportar
  if (exportar) {

    filename <- paste0("Descriptivos_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx")

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Descriptivos")

    # nombres de fila a columna
    resumen_export <- cbind('Estadístico' = row.names(resumen), resumen)
    row.names(resumen_export) <- NULL

    writeData(wb, "Descriptivos", resumen_export)

    # Forzar formato numerico decimal en Excel
    addStyle(wb, "Descriptivos",
             style = createStyle(numFmt = "0.0000"),
             rows = 2:(nrow(resumen_export)+1),
             cols = 2:(ncol(resumen_export)+1),
             gridExpand = TRUE)

    saveWorkbook(wb, filename, overwrite = TRUE)
  }

  invisible(resumen)

}

