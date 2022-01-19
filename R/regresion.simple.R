#' @title Regresión lineal simple.
#'
#' @description Calcula la regresión lineal simple.
#' @usage regresion.simple(x,
#'                  var_depen = NULL,
#'                  var_indepen = NULL,
#'                  introducir = FALSE,
#'                  inferencia = FALSE,
#'                  confianza = 0.95,
#'                  grafico = FALSE,
#'                  exportar = FALSE)
#'
#' @param x Conjunto de datos. Es un dataframe con al menos 2 variables (2 columnas).
#' @param var_depen Es un vector (numérico o carácter) que indica la variable dependiente.
#' @param var_indepen Es un vector (numérico o carácter) que indica la variable independiente.
#' @param introducir Valor lógico. Si \code{introducir = FALSE} (por defecto), el usuario debe indicar el conjunto de datos que desea analizar usando los argumentos \code{x} y/o \code{variable}. Si \code{introducir = TRUE}, se le solicitará al ususario que introduzca la información relevante de las variables: vector de medias y matriz de varianzas-covarianzas.
#' @param inferencia Si \code{inferencia = FALSE}, valor por defecto, se obtienen los resultados de la regresión simple que se estudian en un curso básico de estadística descriptiva (ver referencias de la función). Si \code{inferencia = TRUE}, se obtienen los resultas inferenciales de la regresión.
#' @param confianza Es un valor numérico entre 0 y 1. Indica el nivel de confianza. Por defecto, \code{confianza = 0.95} (95 por ciento)
#' @param grafico Si \code{grafico = TRUE}, se muestran algunos de los principales resultados gráficos de la regresión lineal.
#' @param exportar Para exportar los resultados a una hoja de cálculo Excel (\code{exportar = TRUE}).
#'
#' @return Si \code{inferencia = FALSE}, la función devuelve los principales resultados de la regresión lineal simple que se estudian en estadística descriptiva en un objeto de la clase \code{data.frame}.
#' Si \code{inferencia = TRUE}, la función devuelve los resultados de inferenciales de la regresión. Estos contenidos son estudiados en cursos de inferencia estadística y en temas introductorios de econometría.
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
#' El coeficiente de correlación muestral se obtiene a partir de la siguiente expresión:
#'
#' \if{html}{\figure{correlacion.png}{options: width="50\%" alt="Figure: correlacion.png"}}
#' \if{latex}{\figure{correlacion.png}{options: scale=.5}}
#'
#' Por su construcción, el valor del coeficiente de correlación muestral es el mismo tanto si se calcula a partir de la covarianza y desviaciones típicas muestrales como si se hace a partir de la cuasi-covarianza y cuasi-desviaciones típicas muestrales.
#'
#' @note
#' Si en lugar del tamaño muestral (n) se utiliza el tamaño de la población (N) se obtiene el coeficiente de correlació poblacional:
#'
#' \if{html}{\figure{correlacionpob.png}{options: width="30\%" alt="Figure: correlacionpob.png"}}
#' \if{latex}{\figure{correlacionpob.png}{options: scale=.3}}
#'
#' @seealso \code{\link{matriz.covar}}, \code{\link{matriz.correlacion}}
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
#' # ver código QR
#'
#' @importFrom stats cor
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra arrangeGrob
#' @import dplyr knitr ggplot2
#'
#' @export
regresion.simple <- function(x,
                             var_depen = NULL,
                             var_indepen = NULL,
                             introducir = FALSE,
                             inferencia = FALSE,
                             confianza = 0.95,
                             grafico = FALSE,
                             exportar = FALSE){

  old <- options()
  on.exit(options(old))

  options(scipen = 999)

  if(isFALSE(introducir)){

    x <- data.frame(x)
    varnames <- names(x)

    if(length(x)<2){
      stop("El conjunto de datos seleccionada solo tiene una variable.")
    }

    if(is.null(var_depen) | is.null(var_indepen)){
     stop("Debes seleccionar la variable dependiente y/o independiente")
    }

    if(is.numeric(var_depen)){
      if(var_depen<=length(x)){
        var_depen <- var_depen}
      else{
        stop("Seleccion erronea de variable")
      }
    }

    if(is.character(var_depen)){
      if(var_depen %in% varnames){
        var_depen = match(var_depen,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }


    if(is.numeric(var_indepen)){
      if(var_indepen<=length(x)){
        var_indepen <- var_indepen}
      else{
        stop("Selecci\u00f3n err\u00f3nea de variable")
      }
    }

    if(is.character(var_indepen)){
      if(var_indepen %in% varnames){
        var_indepen = match(var_indepen,varnames)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }

    if(var_depen == var_indepen){
      stop("La variable dependiente e independiente son la misma variable")
    }

      variable <- c(var_indepen,var_depen)


      x <- x[,variable] %>% as.data.frame()
      names(x) <- varnames[variable]
      varnames <- names(x)

    clase <- sapply(x, class)

    if (!all(clase %in% c("numeric","integer"))){
      stop("No puede calcularse la regresi\u00f3 simple porque las variables seleccionadas no son cuantitativas")
    }


  # tabla de resultados parciales

  n <- nrow(x)
  Y <- as.matrix(x[2]) # matriz coeficinetes v.depen
  vx <- as.matrix(x[1]) # matriz coeficientes v.indepen
  vX <- cbind(x0=rep(1,n),vx) # con termino constante

  k = ncol(vX) #columnas de la matriz vX (constante + regresores)


  A <- t(vX)%*%vX
  B <- t(vX)%*%Y

 # C <- A/n # MATRIZ DE MOMENTOS CENTRALES DE ORDEN 2 (RESPECTO AL ORIGEN) ENTRE REGRESORES
 # D <- B/n # MATRIZ DE MOMENTOS CENTRALES DE ORDEN 2 (RESPECTO ORIGEN) ENTRE REGRESANDO Y REGRESORES

  invA <- solve(A)
  coeficientes <- invA%*%B
  names(coeficientes) <- c("constante",varnames[1])

  mediay <- as.numeric(media(Y))
  valores.teoricos <- vX%*%coeficientes
  residuos <- Y - valores.teoricos # observado - estimado


  tabla <- x %>%
    rename(vx=varnames[1],vy=varnames[2]) %>%
    mutate(obs = 1:n,
           vx2 = vx^2,
           vy2 = vy^2,
           vxy = vx * vy,
           valores.teoricos = valores.teoricos,
           errores = Y - valores.teoricos,
           sc.observados = (Y-mediay)^2,
           sc.teoricos = (valores.teoricos - mediay)^2,
           errores2 = errores^2) %>%
    select(obs,everything())

  names(tabla) <- c("id",
                    varnames,
                    paste(varnames[1],"^2",sep=""),
                    paste(varnames[2],"^2",sep=""),
                    paste(varnames[1],"*",varnames[2],sep=""),
                    "valores.teoricos",
                    "errores",
                    "sc.observados",
                    "sc.teoricos",
                    "errores2")

  tabla2 <- tabla %>%
    kable(caption = "c\u00e1lculos intermedios")

  # SUMA DE CUADRADOS
  SCE <- as.numeric(t(residuos)%*%residuos)
  SCT <- sum(Y^2)-n*(mean(Y)^2)
  SCR <- sum(t(coeficientes)%*%B)-n*(mean(Y)^2)


  resumen <- data.frame(observaciones = n,
                        constante = coeficientes[1],
                        coeficiente.regresion = coeficientes[2],
                        suma.cuadrados.total = SCT,
                        suma.cuadrados.regresion = SCR,
                        suma.cuadrados.residuos = SCE,
                        varianza.regresion = SCR/n,
                        varianza.residual = SCE/n,
                        coeficiente.determinacion = SCR/SCT,
                        coeficiente.correlacion = sqrt(SCR/SCT)) %>%
    t() %>% as.data.frame()

  resumen2 <- resumen %>%
    kable(col.names="Valor",
          caption = "Resumen medidas de la regresi\u00f3n")


  } else{   # aqu\u00ed empieza introducir datos

    message("Esta opci\u00f3n obtiene la regresi\u00f3n simple que se estudia en un curso introductorio de estad\u00edstica descriptiva. Ver Esteban y otros (2005).")
    #message("Se deshabilita el argumento: inferencia")

    grafico = FALSE

    print("A continuaci\u00f3n, vas a introducir los datos.")

    mediax <- as.numeric(readline('Introduce el valor de la media de la variable independiente: \n'))
    varx <- as.numeric(readline('Introduce el valor de la varianza muestral de la variable independiente: \n'))
    mediay <- as.numeric(readline('Introduce el valor de la media de la variable dependiente: \n'))
    vary <- as.numeric(readline('Introduce el valor de la varianza muestral de la variable dependiente: \n'))
    covar_xy <- as.numeric(readline('Introduce el valor de la covarianza muestral entre las variables: \n'))

    varnames <- c("X","Y")

    coeficiente.correlacion <- covar_xy/(sqrt(varx)*sqrt(vary))
    coeficiente.determinacion <- covar_xy^2/(varx*vary)


    coeficiente.regresion = round(covar_xy / varx,5)
    constante = round(mediay - coeficiente.regresion * mediax,5)

    coeficientes <- c(constante,coeficiente.regresion)

    resumen <- data.frame() %>%
      summarize(coeficiente.correlacion = round(coeficiente.correlacion,4),
                coeficiente.determinacion = round(coeficiente.determinacion,4),
                varianza.regresion = coeficiente.determinacion * vary,
                varianza.residual = vary - varianza.regresion,
                constante = constante,
                coeficiente.regresion = coeficiente.regresion) %>%
      t() %>% as.data.frame


    resumen2 <- resumen %>%
      kable(col.names="Valor",
            caption="Resultados de la regresi\u00f3n simple")

  }


  if(inferencia){

    if(introducir){
      message("Para obtener los resultados inferenciales de la regresi\u00f3n es necesario el tama\u00f1o de la muestra")
      n <-  as.numeric(readline('Introduce el valor del tama\u00f1o de la muestra: \n'))
      k = 2

      SCT = n *vary
      SCE = (1-coeficiente.determinacion)*SCT
      SCR = SCT - SCE
      varianza.residuos.regresion = SCE/(n-k)
      error.estandard.regresion = sqrt(varianza.residuos.regresion)

      resultados.parciales <- data.frame(valor = c(n,
                               coeficiente.correlacion,
                               coeficiente.determinacion,
                               varianza.residuos.regresion,
                               error.estandard.regresion,
                               SCT,
                               SCR,
                               SCE))


    } else{

      resultados.parciales <- resumen %>% slice(1,10,9,2,3,4,5,6)
      resultados.parciales[4,1] <- SCE/(n-k)
      resultados.parciales[5,1] <- sqrt(SCE/(n-k))

    }

      rownames(resultados.parciales) <- c("n",
                                       "Coeficiente.correlacion",
                                       "Coeficiente.determinacion",
                                       "Varianza.residuos.regresion",
                                       "Error.estandar.regresion",
                                       "suma.cuadrados.total",
                                       "suma.cuadrados.regresion",
                                       "suma.cuadrados.residuos")


      resultados.parciales2 <- resultados.parciales %>%
        kable(col.names="Valor",
              caption="Resumen resultados de la regresi\u00f3n")

      estadistico = (SCR/(k-1))/(SCE/(n-k))
      tabla.anova <- data.frame(Medida = c("Regresion","errores","total"),
                                Suma.cuadrados = round(c(SCR,SCE,SCT),5),
                                gl = c(k-1,n-k,n-1),
                                Media.suma.cuadrados = round(c(SCR/(k-1),SCE/(n-k),NA),5),
                                estadistico.F = round(c(estadistico,NA,NA),5),
                                p_valor = round(c(pf(estadistico,k-1,n-k,lower.tail=FALSE),NA,NA),6))

      tabla.anova[is.na(tabla.anova)] <- " "

      tabla.anova2 <- tabla.anova %>%
        kable(caption = "ANOVA", align= "c")

      # errores tipicos de la regresion
        # VARIANZAS DE LOS ESTIMADORES (BETAS)
      varianza.residuos = SCE/(n-k)

      if(introducir){

        error.tipico.intercepto <- sqrt((SCE/(n-2)) * (1/n + (mediax^2/(n*varx))))
        error.tipico.coef.regresion <- sqrt((SCE/(n-2))/(n*varx))
        error_betas <- c(error.tipico.intercepto,error.tipico.coef.regresion)

      } else{

        var_betas <- varianza.residuos * diag(invA)
        # error estandar de betas
        error_betas <- sqrt(var_betas)

      }

      # valor_t
      t <- coeficientes/error_betas %>% data.frame()

      # p-valores
      p_valor <- 2 * apply(abs(t),1,pt,df=n-k,lower.tail=FALSE)

      # intervalo de confianza
      alfa2 <- (1-confianza)/2
      Lim.inf <- coeficientes - qt(alfa2,n-k,lower.tail = F) * error_betas
      Lim.sup <- coeficientes + qt(alfa2,n-k,lower.tail = F) * error_betas

      # modelo de regresion
      modelo.regresion <- data.frame(coeficientes,
                                     error_betas,
                                     t,
                                     p_valor,Lim.inf,
                                     Lim.sup)

      names(modelo.regresion) <- c("Coeficientes",
                                   "Error t\u00edpico",
                                   "t",
                                   "p-valor",
                                   paste("Lim.inf_",confianza*100,"%",sep=""),
                                   paste("Lim.sup_",confianza*100,"%",sep=""))
      rownames(modelo.regresion) <- c("constante",varnames[1])


      modelo.regresion2 <- modelo.regresion %>%
        kable(caption = "Resultados de la regresi\u00f3n")

      # kk <- list("Resultados globales"=resultados.parciales,
      #             "ANOVA"= tabla.anova,
      #             "modelo.regresion" = modelo.regresion)
      # pander(kk)
    }


    # REPRESENTACION GRAFICA

    if(grafico){

      mediax <- as.numeric(media(x[1]))

      tablaplot <- tabla %>%
        select(1,2,3,7,8,11) %>%
        rename("X"=varnames[1],"Y"=varnames[2]) %>%
        mutate(momento = (X-mediax)^2,
               influencia = 1/n + (momento/sum(momento)),
               puntos.influyentes = ifelse(influencia > 3 * k/n, "influyente", "no influyente"),
               error.norm = errores/(sqrt(sum(errores2)/(n-2))*sqrt(1-influencia)),
               atipico = ifelse(abs(error.norm)>2,"atipico","no atipico"),
               grupo = paste(puntos.influyentes,"_",atipico,sep=""))

      tablaplot$grupo <- factor(tablaplot$grupo,
                                levels=c("influyente_atipico","influyente_no atipico","no influyente_atipico","no influyente_no atipico"))

      #miscolores <- brewer.pal(4,"Set1")
      miscolores <- c("red","purple","chocolate","darkgreen")
      names(miscolores) <- levels(tablaplot$grupo)

      mispuntos <- c(15,18,17,19)
      names(mispuntos) <- levels(tablaplot$grupo)

      tablaplot <- droplevels(tablaplot)

      escalaColor <- scale_color_manual(name = "observaciones",
                                      values = miscolores,
                                      drop = TRUE,
                                      limits= levels(tablaplot$grupo))

      escalaForma <- scale_shape_manual(name = "observaciones",
                                        values = mispuntos,
                                        drop = TRUE,
                                        limits= levels(tablaplot$grupo))

      plot1 <- ggplot(tablaplot,aes(x=X,y=Y)) +
        geom_point(aes(color=grupo,shape=grupo),size=2) +
        escalaColor +
        escalaForma +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE,color="blue") +
        labs(title = "Modelo de regresi\u00f3n estimado",
             subtitle= paste(varnames[2],"=",round(coeficientes[1],5),if_else(coeficientes[2] >=0, "+", ""),round(coeficientes[2],5),"*",varnames[1],sep="")
        )


      plot12 <- ggplot(tablaplot,aes(x=valores.teoricos,y=errores)) +
        geom_point()

      plot21 <- ggplot(tablaplot, aes(x=id,y=valores.teoricos,color=grupo,shape=grupo,label=id)) +
        geom_point() +
        geom_hline(yintercept = mediay) +
      geom_text(data=subset(tablaplot,grupo!='no influyente_no atipico'),
                vjust = -0.75,
                size=2) +
        labs(y="valores pronosticados (teoricos)") +
        escalaColor +
        escalaForma

      plot21


      plot22 <- ggplot(tablaplot, aes(x=id,y=error.norm,color=grupo,shape=grupo,label=id)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_hline(yintercept = 2, linetype=2) +
        geom_hline(yintercept = -2, linetype=2) +
        geom_text(data=subset(tablaplot,grupo!='no influyente_no atipico'),
                  vjust = -0.75,
                  size=2) +
        labs(y="errores estandarizados") +
        escalaColor +
        escalaForma

      #plot <- gridExtra::grid.arrange(plot1,plot12,plot21,plot22, ncol=2, nrow=2)
      plot <- gridExtra::grid.arrange(plot1,gridExtra::arrangeGrob(plot21,plot22), ncol=2)

    }


  if (exportar) {

    names(resumen) <- "Valor"

    filename <- paste("Resultados regresion simple"," (", Sys.time(), ").xlsx", sep = "")
    filename <- gsub(" ", "_", filename)
    filename <- gsub(":", ".", filename)

    if(isFALSE(introducir)){

      if(inferencia){

        names(resultados.parciales) <- "valor"

        lista <- list(tabla,resultados.parciales,tabla.anova,modelo.regresion)

        rio::export(lista, row.names = T, filename, sheetName=c("Resultados parciales",
                                                                "Resumen medidas",
                                                                "ANOVA",
                                                                "Modelo estimado"))
      } else{

        lista <- list(resumen,tabla)

        rio::export(lista, row.names = T, filename, sheetName=c("Resumen",
                                                                "Resultados parciales"))
      }

    } else{

      if(inferencia){

        lista <- list(resultados.parciales,tabla.anova,modelo.regresion)

        rio::export(lista, row.names = T, filename, sheetName=c("Resumen medidas",
                                                                "ANOVA",
                                                                "Modelo estimado"))
      } else{

        rio::export(resumen, row.names = T, filename, sheetName="Resumen")

      }

    }

  }

  if(isFALSE(introducir)){

    if(isFALSE(grafico)){
      plot = NULL
    }

    if(inferencia){

      return(list('Calculos.intermedios' = tabla2,
                  'Resultados.parciales' = resultados.parciales2,
                  'ANOVA' = tabla.anova2,
                  'Moldelo.estimado' = modelo.regresion2,
                  'Graficos' = plot))

    } else{

      return(list('Calculos.intermedios' = tabla2,
                  'Resumen.regresion' = resumen2,
                  'Graficos' = plot))

    }

  } else{

    message("No puede realizarse la representaci\u00f3 gr\u00e1fica si se introducen las medidas num\u00e9ricas")

    if(inferencia){

      return(list('Resultados.parciales' = resultados.parciales2,
                  'ANOVA' = tabla.anova2,
                  'Moldelo.estimado' = modelo.regresion2))

    } else{

      return('Resumen.regresion' = resumen2)

    }

  }

}
