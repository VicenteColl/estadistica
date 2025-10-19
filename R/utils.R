#' Facilita cálculo de cuantiles
#'
#' @description Para obtener los cuantiles
#' @param x Vector
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias
#' @param corte Punto de corte de la distribución de frecuencias
#' @keywords internal
#' @noRd
#' @importFrom stats setNames
.cuantiles.int <- function(x, pesos = NULL, cortes = 0.5){

  if(is.numeric(x)){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse los cuantiles, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    x <- tidyr::drop_na(x)
    #y <- names(x)
    #names(x) <- varnames

    N <- nrow(x)
    tabla <- x %>% group_by(x) %>%
      count() %>%
      ungroup() %>%
      mutate(Ni = cumsum(n))

  } else{

    N <- sum(pesos)

    tabla <- data.frame(x,pesos) %>%
      na.omit
    #y <- names(tabla)
    names(tabla) <- c("x","pesos")

    tabla <- tabla %>%
      arrange(x) %>%
      mutate(Ni = cumsum(pesos))

  }


  cortes <- sort(cortes)

  cuantiles <- c()

  for(i in 1:length(cortes)){

    posicion <- min(which(tabla$Ni >= cortes[i]* N))

    if(cortes[i]* N == tabla$Ni[posicion]){

      cuantil <- mean(c(tabla$x[posicion],tabla$x[posicion+1]),na.rm=TRUE)

    } else {

      cuantil <- tabla$x[posicion]

    }

    cuantiles <- rbind(cuantiles,cuantil)


  }

  cuantiles <- as.data.frame(cuantiles)
  row.names(cuantiles) <- paste(cortes*100,"%",sep="")

  return(cuantiles)
}


#' Facilita cálculo de la mediana
#'
#' @description Para obtener la mediana
#' @param x Vector
#' @param pesos Si los datos de la variable están resumidos en una distribución de frecuencias
#' @noRd
.mediana.int <- function(x, pesos = NULL){

  if(is.numeric(x)){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No puede calcularse la median, alguna variable que has seleccionado no es cuantitativa")
  }

  if(is.null(pesos)){

    x <- tidyr::drop_na(x)
    y <- names(x)
    names(x) <- "x"

    N <- nrow(x)
    tabla <- x %>% group_by(x) %>%
      count() %>%
      ungroup() %>%
      mutate(Ni = cumsum(n))

  } else{

    N <- sum(pesos)

    tabla <- data.frame(x,pesos) %>%
      na.omit
    y <- names(tabla)
    names(tabla) <- c("x","pesos")

    tabla <- tabla %>%
      arrange(x) %>%
      mutate(Ni = cumsum(pesos))

  }

  posicion <- min(which(tabla$Ni >= N/2))

  if(N/2 == tabla$Ni[posicion]){

    mediana <- mean(c(tabla$x[posicion],tabla$x[posicion+1]),na.rm=TRUE)

  } else {

    mediana <- tabla$x[posicion]

  }

  return(mediana)
}

#' Facilita cálculo de los momentos
#'
#' @description Para obtener los momentos
#' @param x Vector
#' @param orden Orden del momento central
#' @keywords internal
#' @noRd
.momento.central <- function(x, orden){

  if(is.numeric(x)){
    varnames <- "variable.x"
  }else{
    varnames <- as.character(names(x))
  }

  x <- data.frame(x)
  names(x) <- varnames

  orden <- as.integer(orden)

  if(!is.integer(orden)){

    stop("El orden del momento central debe ser un valor num\u00e9rico entero")

  }

  x <- data.frame(x)

  clase <- sapply(x, class)

  if (!all(clase %in% c("numeric","integer"))) {
    stop("No pueden calcularse las medidas de forma, alguna variable que has seleccionado no es cuantitativa")
  }

  momento_vacio <- vector("list",length=length(x))

  for(i in 1:length(x)){

    x2 <- x[i] %>% na.omit

    momento <- x2 %>%
      mutate(media_x = media(x2),
             momento = (x2-media_x)^orden) %>%
      summarize(momento = sum(momento)/n()) %>%
      as.numeric()

    momento_vacio[[i]] <- momento

  }

  max_long <-  max(lengths(momento_vacio))
  momento <- sapply(momento_vacio, "[", seq_len(max_long)) %>%
    t() %>%
    as.numeric()
  names(momento) <- varnames

  return(momento)

}

#' Crear hojas excel para exportar resultados de series temporales
#'
#' @description Función para organizar la exportación de resultados de series temporales
#' @param wb Crea el libro de excel
#' @param sheet_name Nombre de la hoja excel
#' @param data Objeto a exportar
#' @keywords internal
#' @noRd
.add_sheet_with_style <- function(wb, sheet_name, data) {
  tryCatch({
    if (is.null(data)) {
      stop(paste("Los datos para la hoja", sheet_name, "son NULL"))
    }

    openxlsx::addWorksheet(wb, sheet_name)

    if (inherits(data, "data.frame")) {
      openxlsx::writeData(wb, sheet_name, data, rowNames = TRUE)

      # Aplicar formato numérico
      numeric_cols <- which(sapply(data, is.numeric))
      if (length(numeric_cols) > 0) {
        openxlsx::addStyle(
          wb, sheet_name,
          style = openxlsx::createStyle(numFmt = "0.0000"),
          rows = 2:(nrow(data) + 1),
          cols = numeric_cols + 1,
          gridExpand = TRUE
        )
      }
    } else if (inherits(data, "lm")) {
      openxlsx::writeData(wb, sheet_name, capture.output(summary(data)))
    } else {
      openxlsx::writeData(wb, sheet_name, as.data.frame(data), rowNames = TRUE)
    }
  }, error = function(e) {
    message("Error al crear la hoja ", sheet_name, ": ", e$message)
  })
}


#' Formatear objetos para visualización en consola
#'
#' @description Elimina notación científica y formatea números para output consistente
#' @param x Objeto a formatear (vector, data.frame, matriz, lista)
#' @param decimales Número de decimales a mostrar (default: 4)
#' @return Objeto formateado para visualización
#' @keywords internal
#' @noRd
.format_consola <- function(x, decimales = 4) {
  if (is.numeric(x)) {
    # Formato fijo sin notación científica para todo el vector
    formatted <- format(x, scientific = FALSE, digits = 12, nsmall = decimales)

    # Limpieza de ceros decimales (vectorizado)
    formatted <- ifelse(
      grepl("\\.0+$", formatted),
      sub("0+$", "0", formatted),  # Mantener un cero si es .0
      sub("\\.?0+$", "", formatted) # Eliminar ceros sobrantes
    )

    return(formatted)
  }

  if (is.data.frame(x) || is.matrix(x)) {
    return(as.data.frame(lapply(x, .format_consola, decimales = decimales)))
  }

  if (is.list(x)) {
    return(lapply(x, .format_consola, decimales = decimales))
  }

  return(x)
}

#' Mostrar resultados formateados en consola (para listas)
#'
#' @description Muestra listas en consola con formato consistente
#' @param x Lista a mostrar
#' @param titulo Título opcional para la salida
#' @param decimales Número de decimales a mostrar
#' @keywords internal
#' @noRd
.mostrar_lista_resultados <- function(x, titulo = NULL, decimales = 4) {
  tryCatch({
    # Verificar que sea una lista
    if (!is.list(x) || inherits(x, "data.frame")) {
      stop("El objeto no es una lista")
    }

    # Mostrar título si se proporciona
    if (!is.null(titulo)) {
      cat("\n", toupper(titulo), "\n", sep = "")
      cat(paste(rep("=", nchar(titulo)), collapse = ""), "\n\n")
    }

    # Formatear y mostrar cada elemento de la lista
    x_formateado <- lapply(x, .format_consola, decimales = decimales)

    # Mostrar cada elemento con su nombre
    for (nombre in names(x_formateado)) {
      cat("$", nombre, "\n", sep = "")
      print(x_formateado[[nombre]], right = FALSE, quote = FALSE)
      cat("\n")
    }

  }, error = function(e) {
    warning("Error al formatear lista: ", e$message)
    print(x)  # Mostrar versión sin formatear como fallback
  })

  invisible(x)
}

#' Comprueba el número el mínimo de frecuencias teóricas en contraste bondad
#'
#' @description Comprueba que las frecuencias teóricas sean mínimo 5
#' @param m Frecuencias teóricas
#' @param n_min_obs Mínimo de frecuencias teóricas para no agrupar
#' @keywords internal
#' @noRd
.check_min_obs <- function(m, n_min_obs = 5) {
  output_matrix <- m
  n <- nrow(output_matrix)

  # Empezamos desde la última fila y vamos hacia arriba
  i <- n
  while (i > 1) {
    # Si el valor en la fila actual es menor a n_min_obs
    if (output_matrix$Freq_esp[i] < n_min_obs) {
      acum_freq <- output_matrix$Freq_esp[i]  # Empezamos la acumulación en Frey_obs
      acum_suma <- output_matrix$Freq_obs[i]      # Empezamos la acumulación en Freq_obs
      j <- i - 1

      # Seguimos hacia arriba sumando hasta que la acumulación sea >= n_min_obs
      while (acum_freq < n_min_obs && j > 0) {
        acum_freq <- acum_freq + output_matrix$Freq_esp[j]  # Acumulamos en Frey_obs
        acum_suma <- acum_suma + output_matrix$Freq_obs[j]      # Acumulamos en SUMA
        j <- j - 1
      }

      # Actualizamos la fila donde terminó la acumulación
      output_matrix$Freq_esp[j + 1] <- acum_freq
      output_matrix$Freq_obs[j + 1] <- acum_suma

      # Eliminamos las filas que se sumaron (entre j + 2 e i)
      if ((j + 2) <= i) {
        output_matrix <- output_matrix[-((j + 2):i), ]
      }

      # Ajustamos el valor de i para continuar desde la fila donde se terminó la acumulación
      i <- j + 1
    } else {
      # Si la fila actual ya es >= n_min_obs, seguimos con la fila anterior
      i <- i - 1
    }
  }

  return(output_matrix)
}

#' Función interna que combina o resume data frames estadísticos
#' @keywords internal
#' @noRd
#' @import tibble
.resumir <- function(...,
                     nombres = NULL,
                     exportar = FALSE,
                     archivo = NULL) {

  # Captura robusta de argumentos
  listas <- list(...)
  llamadas <- as.list(substitute(list(...)))[-1L]

  # Nombres automáticos si no se pasan
  if (is.null(nombres)) {
    nombres <- names(llamadas)
    if (is.null(nombres) || any(nombres == "")) {
      nombres <- sapply(llamadas, deparse)
    }
  }

  if (length(nombres) != length(listas)) {
    stop("Error interno: no se pudieron asignar los nombres a todos los objetos.")
  }

  # Preparar dataframes y conservar rownames
  listas <- lapply(listas, function(df) {
    rn <- rownames(df)
    df <- as.data.frame(unclass(df)) # limpiar clases
    df$estadístico <- if (!is.null(rn)) rn else seq_len(nrow(df))
    df
  })

  # Comprobamos si las columnas coinciden (excepto "estadístico")
  nombres_col <- lapply(listas, names)
  nombres_col <- lapply(nombres_col, function(x) setdiff(x, "estadístico"))
  mismas_columnas <- length(unique(nombres_col)) == 1

  # --- CASO 1: columnas coinciden ---
  if (mismas_columnas) {
    tablas_etiquetadas <- Map(function(df, nom) {
      df$medida <- as.character(nom)
      df
    }, listas, nombres)

    tabla_final <- dplyr::bind_rows(tablas_etiquetadas)
    tabla_final <- dplyr::select(tabla_final, "medida", "estadístico", dplyr::everything())

    # Exportar a Excel
    if (isTRUE(exportar)) {
      if (is.null(archivo)) {
        archivo <- paste0("resultados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      }
      openxlsx::write.xlsx(tabla_final, file = archivo, rowNames = FALSE)
      message("✅ Resultados exportados a: ", archivo)
    }

    return(tabla_final)
  }

  # --- CASO 2: columnas distintas ---
  estructura <- Map(function(df, nom) {
    df_clean <- as.data.frame(unclass(df))  # limpiar clases

    # Tomamos la última columna como 'estadístico' y la ponemos al principio
    n_col <- ncol(df_clean)
    df_clean <- cbind(estadístico = df_clean[[n_col]], df_clean[, -n_col, drop = FALSE])

    # Mostramos todas las filas (sin usar head)
    preview_df <- df_clean

    list(
      medida = nom,
      preview = preview_df
    )
  }, listas, nombres)

  names(estructura) <- nombres
  class(estructura) <- "resumen_resultados"

  # Método de impresión que muestra todas las filas
  print.resumen_resultados <- function(x, ...) {
    cat("Resumen de objetos estadísticos:\n\n")
    for (i in seq_along(x)) {
      obj <- x[[i]]
      cat("•", obj$medida, "\n")
      cat("  - Resultados:\n")
      print(obj$preview, row.names = FALSE)
      cat("\n")
    }
    invisible(x)
  }

  assign("print.resumen_resultados", print.resumen_resultados, envir = parent.frame())

  # Exportar Excel en caso de columnas distintas
  if (isTRUE(exportar)) {
    if (is.null(archivo)) {
      archivo <- paste0("resumen_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    }
    wb <- openxlsx::createWorkbook()
    for (i in seq_along(estructura)) {
      sheet_name <- substr(names(estructura)[i], 1, 31)
      openxlsx::addWorksheet(wb, sheetName = sheet_name)
      openxlsx::writeData(wb, sheet = i, x = estructura[[i]]$preview)
    }
    openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
    message("✅ Resumen exportado a: ", archivo)
  }

  return(estructura)
}

#' Combina o resume data frames para resumen estadístico
#'
#' @param ... Data frames a combinar o resumir.
#' @param nombres Vector de nombres opcionales para los objetos.
#' @param exportar Logical. Si TRUE, exporta a Excel.
#' @param archivo Nombre del archivo Excel; si NULL, se genera automáticamente.
#' @return Data frame combinado si las columnas coinciden, o lista resumen si son distintas.
#' @examples
#' \dontrun{
#' Ejemplo caso 1: resumir varios data frames con las mismas columnas
#' media <- estadistica::media(mtcars)
#' cuantiles <- estadistica::cuantiles(mtcars)
#'
#' resultados <- resumir(media, cuantiles, export=TRUE)
#' resultados
#'
#' Ejemplo caso 2: resumir varios data frames con distintas columnas
#' forma <- estadistica::medidas.forma(mtcars[c(1, 3, 4)])
#' resultados <- resumir(media, cuantiles, forma, export=TRUE)
#' resultados
#' }

#' @export
resumir <- .resumir
