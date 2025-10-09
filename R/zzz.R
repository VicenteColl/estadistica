.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cli", quietly = TRUE)) {
    # Mensaje en azul
    cli::cli_alert_info(col_blue("Este paquete est\u00e1 en desarrollo. Por favor, si detectas errores o quieres hacernos alguna sugerencia, cont\u00e1ctanos."))

    # Enlace al canal de YouTube
    cli::cli_text(cli::col_blue("Tambi\u00e9n puedes seguirnos en el canal de YouTube: {cli::style_hyperlink('https://go.uv.es/estadistic/youtube', 'https://go.uv.es/estadistic/youtube')}"))

    cli::cli_alert_info(col_blue("Este paquete se desarrolla como parte de un proyecto de innovaci\u00f3n educativa de la Universidad de Valencia."))


  } else {
    # Version alternativa sin cli
    packageStartupMessage("Este paquete est\u00e1 en desarrollo. Visita nuestro canal: https://go.uv.es/estadistic/youtube")
  }
}

# Funcion auxiliar para color azul
col_blue <- function(x) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::col_blue(x)
  } else {
    x
  }
}
