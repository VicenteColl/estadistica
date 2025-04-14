#' @title Regresión lineal simple.
#'
#' @description Calcula la regresión lineal simple.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrregresion1.png}{options: width="25\%" alt="Figure: qrmuestra1.png"}}
#' \if{latex}{\figure{qrregresion1.png}{options: width=3cm}}
#'
#' \if{html}{\figure{qrregresion2.png}{options: width="25\%" alt="Figure: qrmuestra1.png"}}
#' \if{latex}{\figure{qrregresion2.png}{options: width=3cm}}

#'
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
#' Facultad de Economía. Universidad de Valencia (España)
#'
#' @details
#'
#' Se obtiene la recta de regresión minimocuadrática de Y (variable dependiente) en función de X (variable independiente).
#' La recta de regresión puede expresarse como:
#'
#' \if{html}{\figure{regresion1.png}{options: width="50\%" alt="Figure: regresion1.png"}}
#' \if{latex}{\figure{regresion1.png}{options: width=7cm}}
#'
#' o alternativamente:
#'
#' \if{html}{\figure{regresion2.png}{options: width="75\%" alt="Figure: regresion2"}}
#' \if{latex}{\figure{regresion2.png}{options: width=10cm}}
#'
#' En las representaciones gráficas las observaciones anómals se detectan a partir del punto leverage:
#'
#' \if{html}{\figure{influyente.png}{options: width="50\%" alt="Figure: influyente.png"}}
#' \if{latex}{\figure{influyente.png}{options: width=6cm}}
#'
#' de forma que una observación tendrá efecto de apalancamiento si:
#'
#' \if{html}{\figure{obsinfluyente.png}{options: width="25\%" alt="Figure: obsinfluyente.png"}}
#' \if{latex}{\figure{obsinfluyente.png}{options: width=2.5cm}}
#'
#'  donde p=2 (en el caso de la regresión simple). En general, p es igual al número de variables independientes más la constante.
#'
#' Por otra parte, las observaciones atípicas se identifican a partir de los errores estandarizados (se). Estos errores se obtienen a partir de:
#'
#' \if{html}{\figure{errorstandarizado.png}{options: width="45\%" alt="Figure: errorstandarizado.png"}}
#' \if{latex}{\figure{errorstandarizado.png}{options: width=6cm}}
#'
#' Una observación será atípica si:
#'
#' \if{html}{\figure{obsatipica.png}{options: width="20\%" alt="Figure: obsatipica.png"}}
#' \if{latex}{\figure{obsatipica.png}{options: width=2cm}}
#'
#'  @seealso \code{\link{matriz.covar}}, \code{\link{matriz.correlacion}}
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
#' ## Not run:
#' ejemplo_regresion <- regresion.simple(turistas,
#' var_depen=2,var_indepen=3,grafico=TRUE)
#' ## End(Not run)
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
#  on.exit(options(old))

  options(scipen=999, digits=4)

  if(isFALSE(introducir)){

    varnames <- as.character(names(x))
    x <- data.frame(x)
    names(x) <- varnames


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


      x <- x[,variable] %>% as.matrix
      colnames(x) <- varnames[variable]
      varnames <- colnames(x)

    clase <- sapply(x, class)

    if (!all(clase %in% c("numeric","integer"))){
      stop("No puede calcularse la regresi\u00f3 simple porque las variables seleccionadas no son cuantitativas")
    }


  # tabla de resultados parciales

  n <- nrow(x)
  Y <- matrix(x[,2],ncol=1) # matriz coeficinetes v.depen
  vx <- matrix(x[,1],ncol=1) # matriz coeficientes v.indepen
  vX <- cbind(x0=rep(1,n),vx) # con termino constante

  k = ncol(vX) #columnas de la matriz vX (constante + regresores)


  A <- t(vX)%*%vX
  B <- t(vX)%*%Y

 # C <- A/n # MATRIZ DE MOMENTOS CENTRALES DE ORDEN 2 (RESPECTO AL ORIGEN) ENTRE REGRESORES
 # D <- B/n # MATRIZ DE MOMENTOS CENTRALES DE ORDEN 2 (RESPECTO ORIGEN) ENTRE REGRESANDO Y REGRESORES

  invA <- solve(A)
  coeficientes <- invA%*%B
  colnames(coeficientes) <- "Parametros"
  rownames(coeficientes) <- c("constante",varnames[1])

  mediay <- mean(Y)
  valores.teoricos <- vX%*%coeficientes
  colnames(valores.teoricos) <- "Valores.teoricos"

  residuos <- Y - valores.teoricos # observado - estimado
  colnames(residuos) <- "errores"

  sc.observados = (Y-mediay)^2
  colnames(sc.observados) <- "sc.observados"

  sc.teoricos = (valores.teoricos - mediay)^2
  colnames(sc.teoricos) <- "sc.teoricos"

  errores2 = residuos^2
  colnames(errores2) <- "errores2"

  tabla <- cbind(x,
                 obs = 1:n,
                 vx2 = x[,1]^2,
                 vy2 = x[,2]^2,
                 vxy = x[,1] * x[,2],
                 valores.teoricos,
                 residuos,
                 sc.observados,
                 sc.teoricos,
                 errores2) %>%
    as.data.frame() %>%
    round(4) %>%
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
                    "errores^2")

  tabla2 <- tabla %>%
    kable(caption = "c\u00e1lculos intermedios")

  # SUMA DE CUADRADOS
  SCE <- t(residuos)%*%residuos %>% as.numeric()
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
                        coeficiente.correlacion = correlacion(as.data.frame(x)) ) %>%
    t() %>%
    round(4) %>%
    as.data.frame()

  names(resumen) <- "Resumen"

  resumen2 <- resumen %>%
    kable(col.names="Valor",
          caption = "Resumen medidas de la regresi\u00f3n")


  # deteccion de outliers
  mediax <- as.numeric(media(x[,1]))

  tablaplot <- tabla %>%
    select(1,2,3,7,8,11) %>%
    rename("X"=varnames[1],"Y"=varnames[2]) %>%
    mutate(momento = (X-mediax)^2,
           leverage = 1/n + (momento/sum(momento)),
           puntos.palanca = ifelse(leverage > 3 * k/n, "palanca", "no palanca"),
           error.norm = errores/(sqrt(sum(errores2)/(n-2))*sqrt(1-leverage)),
           atipico = ifelse(abs(error.norm)>2,"atipico","no atipico"),
           distancia.cook = (error.norm^2 / 2) * (leverage /(1-leverage)),
           grupo = paste(puntos.palanca,"_",atipico,sep=""))

  deteccion.outliers <- tablaplot[c(1,8,10,12)]


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
      t() %>%
      round(4) %>%
      as.data.frame()

    names(resumen) <- "Resumen"


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

      resultados.parciales <- resumen %>% slice(1,10,9,7,8,4,5,6)
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
      p_valor <- round(p_valor,6)

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

      tablaplot$grupo <- factor(tablaplot$grupo)
      levels(tablaplot$grupo) <- list(palanca_atipico = "palanca_atipico",
                                      palanca_no.atipico = "palanca_no atipico",
                                      no.palanca_atipico = "no palanca_atipico",
                                      normal = "no palanca_no atipico")

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

      plot1 <- ggplot(tablaplot,aes(x=X,y=Y,label=id)) +
        geom_point(aes(color=grupo,shape=grupo),size=2) +
        escalaColor +
        escalaForma +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE,color="blue") +
        geom_text(data=subset(tablaplot,grupo!='normal'),
                  vjust = -0.7,
                  size = 2.5) +
        labs(title = "Modelo de regresi\u00f3n estimado",
             subtitle= paste(varnames[2],"=",round(coeficientes[1],5),if_else(coeficientes[2] >=0, "+", ""),round(coeficientes[2],5),"*",varnames[1],sep=""),
             x = varnames[1],
             y = varnames[2]) +
        theme_classic() +
        theme(legend.title = element_blank(),
              legend.key.size = unit(0, 'lines'),
              legend.position = "bottom",
              legend.text = element_text(size = 7))


      #plot12 <- ggplot(tablaplot,aes(x=valores.teoricos,y=errores)) +
      #  geom_point()

      plot21 <- ggplot(tablaplot, aes(x=id,y=valores.teoricos,color=grupo,shape=grupo,label=id)) +
        geom_point() +
        geom_hline(yintercept = mediay) +
      geom_text(data=subset(tablaplot,grupo!='normal'),
                vjust = -0.55,
                size= 2.5) +
        labs(y="valores pronosticados (teoricos)") +
        escalaColor +
        escalaForma +
        theme_classic()+
        theme(legend.position = "none")


      plot22 <- ggplot(tablaplot, aes(x=id,y=error.norm,color=grupo,shape=grupo,label=id)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_hline(yintercept = 2, linetype=2) +
        geom_hline(yintercept = -2, linetype=2) +
        geom_text(data=subset(tablaplot,grupo!='normal'),
                  vjust = -0.7,
                  size = 2.5) +
        labs(y="errores estandarizados") +
        escalaColor +
        escalaForma +
        theme_classic()+
        theme(legend.position = "none")

      #plot <- gridExtra::grid.arrange(plot1,plot12,plot21,plot22, ncol=2, nrow=2)
      plot <- gridExtra::grid.arrange(plot1,gridExtra::arrangeGrob(plot21,plot22), ncol=2)

    }

  if (exportar) {
    # Prepara el nombre del archivo
    filename <- paste("Resultados_regresion_simple_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".xlsx", sep = "")

    # Crear el workbook
    wb <- openxlsx::createWorkbook()

    # Función auxiliar para añadir data.frames con formato
    add_sheet_with_style <- function(wb, sheet_name, data) {
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, data, rowNames = TRUE)

      # Aplicar formato numérico a columnas numéricas
      numeric_cols <- which(sapply(data, is.numeric))
      if (length(numeric_cols) > 0) {
        openxlsx::addStyle(
          wb, sheet_name,
          style = openxlsx::createStyle(numFmt = "0.0000"),
          rows = 2:(nrow(data) + 1),
          cols = numeric_cols + 1,  # +1 porque rowNames=TRUE añade una columna
          gridExpand = TRUE
        )
      }
    }

    if (isFALSE(introducir)) {
      if (inferencia) {
        names(resultados.parciales) <- "valor"

        # Añadir todas las hojas
        add_sheet_with_style(wb, "Resultados_parciales", tabla)
        add_sheet_with_style(wb, "Resumen_medidas", resultados.parciales)
        add_sheet_with_style(wb, "ANOVA", tabla.anova)

        # Hoja para el modelo (manejo especial)
        openxlsx::addWorksheet(wb, "Modelo_estimado")
        openxlsx::writeData(wb, "Modelo_estimado", capture.output(modelo.regresion))

      } else {
        add_sheet_with_style(wb, "Resumen", resumen)
        add_sheet_with_style(wb, "Resultados_parciales", tabla)
      }

    } else {
      if (inferencia) {
        add_sheet_with_style(wb, "Resumen_medidas", resultados.parciales)
        add_sheet_with_style(wb, "ANOVA", tabla.anova)

        # Hoja para el modelo (manejo especial)
        openxlsx::addWorksheet(wb, "Modelo_estimado")
        openxlsx::writeData(wb, "Modelo_estimado", capture.output(modelo.regresion))

      } else {
        add_sheet_with_style(wb, "Resumen", resumen)
      }
    }

    # Guardar el archivo
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  }

  if(isFALSE(introducir)){

    if(isFALSE(grafico)){
      plot = NULL
    }

    if(inferencia){

      return(list('Calculos.intermedios' = tabla,
                  'Resultados.parciales' = resultados.parciales,
                  'ANOVA' = tabla.anova,
                  'Modelo.estimado' = modelo.regresion,
                  'Deteccion.outliers' = deteccion.outliers,
                                    'Graficos' = plot))

    } else{

      return(list('Calculos.intermedios' = tabla,
                  'Resumen.regresion' = resumen,
                  'Deteccion.outliers' = deteccion.outliers,
                  'Graficos' = plot))

    }

  } else{

    message("No puede realizarse la representaci\u00f3 gr\u00e1fica si se introducen las medidas num\u00e9ricas")

    if(inferencia){

      return(list('Resultados.parciales' = resultados.parciales,
                  'ANOVA' = tabla.anova2,
                  'Moldelo.estimado' = modelo.regresion))

    } else{

      return('Resumen.regresion' = resumen)

    }

  }

  on.exit(options(old))

}
