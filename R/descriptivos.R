#' @title Resumen descriptivos.
#'
#' @description Calcula un resumen de los principales estadísticos descriptivos.
#'
#' Lee el código QR para video-tutorial sobre el uso de la función con un ejemplo.
#'
#' \if{html}{\figure{qrdescriptivos.png}{options: style="width: 25\%;"}}
#' \if{latex}{\figure{qrdescriptivos.png}{options: width=3cm}}
#'
#' @param data Conjunto de datos. Puede ser un vector o un dataframe.
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
resumen.descriptivos <- function(data,
                                 variable = NULL,
                                 pesos = NULL,
                                 exportar = FALSE) {

  # nombres
  get_name_from_expr <- function(expr) {

    if (is.name(expr)) return(as.character(expr))

    if (is.character(expr)) return(expr)

    if (is.call(expr)) {

      fn <- as.character(expr[[1]])

      if (fn %in% c("$", "[[") && length(expr) >= 3) {

        el <- expr[[3]]

        if (is.name(el) || is.character(el)) {
          return(as.character(el))
        }

        return(deparse(el)[1])
      }

      if (fn == "[") {

        for (k in seq_along(expr)[-1]) {

          el <- expr[[k]]

          if (is.character(el)) return(as.character(el))

          if (is.name(el)) return(as.character(el))
        }
      }

      if (fn == "(" && length(expr) >= 2) {
        return(get_name_from_expr(expr[[2]]))
      }
    }

    txt <- paste(deparse(expr), collapse = "")
    txt <- trimws(txt)

    m <- regmatches(
      txt,
      regexpr("([A-Za-z0-9_.]+)\\s*$", txt)
    )

    if (length(m) >= 1 && nzchar(m[1])) {
      return(m[1])
    }

    return(txt)
  }

  var_quo   <- rlang::enquo(variable)
  pesos_quo <- rlang::enquo(pesos)

  expr_data   <- substitute(data)
  nombre_data <- get_name_from_expr(expr_data)

  # convertir a dataframe
  if (!is.data.frame(data)) {

    data <- as.data.frame(data)

    if (ncol(data) == 1) {

      nm <- names(data)[1]

      if (is.null(nm) || nm %in% c("", "x", "X", "V1")) {
        names(data)[1] <- nombre_data
      }
    }
  }

  # Seleccion de variables
  if (rlang::quo_is_null(var_quo)) {

    selected_vars <- names(data)[sapply(data, is.numeric)]

  } else {

    tryCatch({

      selected_vars <- names(
        tidyselect::eval_select(
          expr = var_quo,
          data = data
        )
      )

    }, error = function(e) {

      eval_res <- rlang::eval_tidy(
        var_quo,
        env = rlang::caller_env()
      )

      if (is.numeric(eval_res)) {

        selected_vars <- names(data)[eval_res]

      } else if (is.character(eval_res)) {

        selected_vars <- eval_res

      } else {

        stop("Selecci\u00f3n de variables no v\u00e1lida")
      }
    })
  }

  if (length(selected_vars) == 0) {
    stop("No hay variables num\u00e9ricas seleccionadas")
  }

  # Pesos
  if (!rlang::quo_is_null(pesos_quo)) {

    tryCatch({

      peso_sel <- tidyselect::eval_select(
        expr = pesos_quo,
        data = data
      )

      peso_name <- names(peso_sel)[1]

    }, error = function(e) {

      eval_res <- rlang::eval_tidy(
        pesos_quo,
        env = rlang::caller_env()
      )

      if (is.numeric(eval_res)) {

        peso_name <- names(data)[eval_res]

      } else if (is.character(eval_res)) {

        peso_name <- eval_res

      } else {

        stop("Pesos no v\u00e1lidos")
      }
    })

  } else {

    peso_name <- NULL
  }

  # Funcion auxiliar para organizar resultados
  to_wide_df <- function(res) {

    if (!is.data.frame(res)) {
      res <- as.data.frame(res)
    }

    for (v in selected_vars) {

      if (!(v %in% names(res))) {
        res[[v]] <- NA
      }
    }

    res <- res[, selected_vars, drop = FALSE]

    for (j in seq_len(ncol(res))) {
      res[[j]] <- as.numeric(as.character(res[[j]]))
    }

    rownames(res) <- NULL

    return(res)
  }

  # -Medidas basicas
  if (!is.null(peso_name)) {

    media_raw <- media(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo
    )

    varianza_raw <- varianza(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo
    )

    desviacion_raw <- desviacion(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo
    )

    coef_raw <- coeficiente.variacion(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo
    )

    forma_raw <- medidas.forma(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo
    )

  } else {

    media_raw <- media(
      data,
      variable = !!var_quo
    )

    varianza_raw <- varianza(
      data,
      variable = !!var_quo
    )

    desviacion_raw <- desviacion(
      data,
      variable = !!var_quo
    )

    coef_raw <- coeficiente.variacion(
      data,
      variable = !!var_quo
    )

    forma_raw <- medidas.forma(
      data,
      variable = !!var_quo
    )
  }

  media_df      <- to_wide_df(media_raw)
  varianza_df   <- to_wide_df(varianza_raw)
  desviacion_df <- to_wide_df(desviacion_raw)
  coef_df       <- to_wide_df(coef_raw)

  # Medidas de forma
  forma_df <- to_wide_df(forma_raw)

  if (is.data.frame(forma_raw) &&
      nrow(forma_raw) >= 2) {

    asimetria_df <- forma_df[1, , drop = FALSE]
    curtosis_df  <- forma_df[2, , drop = FALSE]

  } else {

    vacio <- to_wide_df(
      data.frame(
        matrix(
          NA,
          nrow = 1,
          ncol = length(selected_vars)
        )
      )
    )

    asimetria_df <- vacio
    curtosis_df  <- vacio
  }

  # Cuantiles
  probs <- c(0, 0.25, 0.5, 0.75, 1)

  if (!is.null(peso_name)) {

    cuantiles_df <- cuantiles(
      data,
      variable = !!var_quo,
      pesos = !!pesos_quo,
      cortes = probs
    )

  } else {

    cuantiles_df <- cuantiles(
      data,
      variable = !!var_quo,
      cortes = probs
    )
  }

  cuantiles_df <- as.data.frame(cuantiles_df)

  names(cuantiles_df) <- selected_vars

  for (j in seq_len(ncol(cuantiles_df))) {

    cuantiles_df[[j]] <- as.numeric(
      as.character(cuantiles_df[[j]])
    )
  }

  minimo_df  <- cuantiles_df[1, , drop = FALSE]
  q1_df      <- cuantiles_df[2, , drop = FALSE]
  mediana_df <- cuantiles_df[3, , drop = FALSE]
  q3_df      <- cuantiles_df[4, , drop = FALSE]
  maximo_df  <- cuantiles_df[5, , drop = FALSE]

  ric_df <- q3_df - q1_df

  # Modas
  modas_por_var <- list()

  max_modas <- 0

  for (v in selected_vars) {

    if (!is.null(peso_name)) {

      tmp <- data.frame(
        tmp = data[[v]],
        w   = data[[peso_name]]
      )

      names(tmp)[1] <- v

      moda_res <- tryCatch(

        moda(
          tmp,
          variable = !!rlang::sym(v),
          pesos = !!rlang::sym("w")
        ),

        error = function(e) {
          data.frame(moda = NA_real_)
        }
      )

    } else {

      moda_res <- tryCatch(

        moda(
          data[v],
          variable = !!rlang::sym(v)
        ),

        error = function(e) {
          data.frame(moda = NA_real_)
        }
      )
    }

    if (is.data.frame(moda_res) &&
        nrow(moda_res) > 0) {

      modas <- moda_res[, 1]

    } else {

      modas <- NA_real_
    }

    modas_por_var[[v]] <- modas

    max_modas <- max(
      max_modas,
      length(modas)
    )
  }

  modas_df_list <- list()

  for (i in 1:max_modas) {

    row_i <- lapply(selected_vars, function(v) {

      m <- modas_por_var[[v]]

      if (i <= length(m)) {
        m[i]
      } else {
        NA_real_
      }
    })

    modas_df_list[[i]] <- as.data.frame(
      t(unlist(row_i))
    )

    names(modas_df_list[[i]]) <- selected_vars
  }

  modas_final <- do.call(
    rbind,
    modas_df_list
  )

  rownames(modas_final) <- paste0(
    "moda_",
    1:max_modas
  )

  # Pegar resultados finales
  filas <- list(
    media            = media_df,
    minimo           = minimo_df,
    cuartil1         = q1_df,
    mediana          = mediana_df,
    cuartil3         = q3_df,
    maximo           = maximo_df,
    RIC              = ric_df,
    varianza         = varianza_df,
    desviacion       = desviacion_df,
    coef_variacion   = coef_df,
    asimetria        = asimetria_df,
    curtosis         = curtosis_df
  )

  resultado <- NULL

  for (nm in names(filas)) {

    df <- filas[[nm]]

    if (is.null(df) || nrow(df) == 0) {
      next
    }

    df <- as.data.frame(df)

    for (j in seq_len(ncol(df))) {

      df[[j]] <- as.numeric(
        as.character(df[[j]])
      )
    }

    rownames(df) <- nm

    resultado <- rbind(
      resultado,
      df
    )
  }

  resultado <- rbind(
    resultado,
    modas_final
  )

  resultado <- round(resultado,4)

  #Excel
  if (exportar) {

    filename <- paste0(
      "Descriptivos_",
      format(Sys.time(), "%Y-%m-%d_%H.%M.%S"),
      ".xlsx"
    )

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(
      wb,
      "Descriptivos"
    )

    resumen_export <- cbind(
      Estadistico = rownames(resultado),
      resultado
    )

    rownames(resumen_export) <- NULL

    openxlsx::writeData(
      wb,
      "Descriptivos",
      resumen_export
    )

    openxlsx::addStyle(
      wb,
      "Descriptivos",
      style = openxlsx::createStyle(
        numFmt = "0.0000"
      ),
      rows = 2:(nrow(resumen_export) + 1),
      cols = 2:(ncol(resumen_export) + 1),
      gridExpand = TRUE
    )

    openxlsx::saveWorkbook(
      wb,
      filename,
      overwrite = TRUE
    )
  }

  class(resultado) <- c("resumen",class(resultado))

  return(resultado)
}
