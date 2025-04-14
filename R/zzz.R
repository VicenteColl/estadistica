.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cli", quietly = TRUE)) {
    # Mensaje en azul
    cli::cli_alert_info(col_blue("Este paquete está en desarrollo. Por favor, si detectas errores o quieres hacernos alguna sugerencia, contáctanos."))

    # Enlace al canal de YouTube
    cli::cli_text(cli::col_blue("También puedes seguirnos en el canal de YouTube: {cli::style_hyperlink('https://go.uv.es/estadistic/youtube', 'https://go.uv.es/estadistic/youtube')}"))

    cli::cli_alert_info(col_blue("Este paquete se desarrolla como parte de un proyecto de innovación educativa de la Universidad de Valencia."))


  } else {
    # Versión alternativa sin cli
    packageStartupMessage("Este paquete está en desarrollo. Visita nuestro canal: https://go.uv.es/estadistic/youtube")
  }
}

# Función auxiliar para color azul
col_blue <- function(x) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::col_blue(x)
  } else {
    x
  }
}
