load_theme <- function(
  x = "default"
){
  stopifnot(is_scalar_character(x))

  named_themes <- list(
    "default" = getOption("shed.theme"),
    "light"   = system.file("css", "shed_dark.css", package = "shed"),
    "dark"    = system.file("css", "shed_dark.css", package = "shed")
  )

  if (is_css_file(x)){
    flog.trace("Reading theme from %s", x)
    return(read_css_theme(x))

  } else if (is_scalar_character(x) && x %in% names(named_themes)){
    flog.trace("Reading internal theme from %s", named_themes[[x]])
    return(read_css_theme(named_themes[[x]]))

  } else if (contains_newline(x)) {
    flog.trace("Theme directly supplied as character scalar")
    return(x)

  } else {
    flog.warn("Cannot parse theme. Falling back to default")
    read_css_theme(getOption("shed.css", system.file("css", "shed_dark.css", package = "shed")))
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
