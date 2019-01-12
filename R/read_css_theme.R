#' load a css theme
#'
#' @param theme A `character` scalar that contains one of:
#'    * The name of one of the internal preset themes :`default`, `"light"`, `"dark"`.
#'    * A path to a `css` file (must have the file extension `.css`)
#'    * `css` style as character string (must contain at least one newline)
#'
load_theme <- function(
  theme = "default"
){
  stopifnot(is_scalar_character(theme))

  named_themes <- list(
    "default" = getOption("shed.theme"),
    "light"   = system.file("css", "shed_dark.css", package = "shed"),
    "dark"    = system.file("css", "shed_dark.css", package = "shed")
  )

  if (is_css_file(theme)){
    lg$trace("Reading theme", file = theme)
    return(read_css_theme(theme))

  } else if (is_scalar_character(theme) && theme %in% names(named_themes)){
    lg$trace("Using internal theme", theme = theme)
    return(read_css_theme(named_themes[[theme]]))

  } else if (contains_newline(theme)) {
    lg$trace("Theme directly supplied as character scalar")
    return(theme)

  } else {
    lg$warn("Cannot parse theme. Falling back to default")
    read_css_theme(getOption(
      "shed.css",
      system.file("css", "shed_dark.css", package = "shed")
    ))
  }
}




read_css_theme <- function(
  x,
  font_size = getOption("shed.font_size", 14L)
){
  paste(
    paste(readLines(x), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", font_size)
  )
}




contains_newline <- function(x){
  any(grepl("(\r\n|\r|\n)", x))
}
