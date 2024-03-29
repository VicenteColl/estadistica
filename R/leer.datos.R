#' @title Leer datos.
#'
#' @description Carga un conjunto de datos.
#'
#' \if{html}{\figure{qrleerdatos.png}{options: width="25\%" alt="Figure: qrleerdatos.png"}}
#' \if{latex}{\figure{qrleerdatos.png}{options: width=3cm}}
#'
#' @usage leer.datos(introducir = FALSE, pos = 1)
#'
#' @param introducir Valor lógico. Si \code{introducir = } (por defecto), se abrirá una ventana para que el usuario seleccione el fichero de datos que quiere cargar. Si \code{introducir = TRUE}, el usuario introducirá él mismo los datos.
#' @param pos Es un valor fijo utilizado para mostrar el dataframe del usuario en el Global Environment.
#'
#' @return Al finalizar el proceso de lectura de datos se mostrará el dataframe cargado en el environment.
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
#' @references
#' Esteban García, J. et al. (2005). Estadística descriptiva y nociones de probabilidad. Thomson.
#'
#' @importFrom stats na.omit
#'
#' @import dplyr
#'
#' @export
leer.datos <- function(introducir = FALSE, pos=1){

  if(pos!=1){
    stop("No cambies el valor del argumento pos.")
  }

  if(isFALSE(introducir)){

    print("Selecciona el fichero de datos (excel, csv, txt, etc.) con el que quieres trabajar ")

    x <- rio::import(file.choose())
    name_df <- readline(prompt = "Introduce el nombre que quieres darle a tu dataframe: ")
    assign(name_df,x,envir=as.environment(pos))


  } else{
    print("Esta opci\u00f3n es v\u00e1lida si vas a introducir pocos datos.")

    numero_var <- readline(prompt = "\u00bflos datos de cu\u00e1ntas variables quieres introducir? Indica el n\u00famero: ")
    numero_var <- as.numeric(numero_var)

    print("Cuando termines de introducir tus datos pulsa ENTER")
    print("NO UTILICES ESPACIOS EN BLANCO EN LOS NOMBRES DE VARIABLES O DATAFRAME. SI NECESITAS SEPARAR PALABRAS UTILIZA . o _")


    if(numero_var == 1){

      x <- scan()
      variable <- readline(prompt = "Introduce el nombre que le quieres dar a la variable: ")
      x <- as.data.frame(x)
      names(x) <- variable
      name_df <- readline(prompt = "Introduce el nombre que quieres darle a tu dataframe: ")
      assign(name_df,x,envir=as.environment(pos))


    } else{


      lista <- vector(mode="list", length = numero_var)
      n <- c()

      for(i in 1:numero_var){

        print(paste("Introduce los datos de la variable ",i,sep=""))
        var <- scan()
        variable <- readline(prompt = paste("Introduce el nombre que le quieres dar a la variable ",i," :",sep=""))

        lista[[i]] <- var
        names(lista)[i] <- variable
        n[i] <- length(var)

        n <- c(n,n[i])

      }

      df <- lapply(lista,'length<-',max(n))
      attributes(df) <- list(names = names(df),
                             row.names = 1:max(n),
                             class = "data.frame")
      name_df <- readline(prompt = "Introduce el nombre que quieres darle a tu dataframe: ")
      assign(name_df,df,envir=as.environment(pos))


    }
  }
}



