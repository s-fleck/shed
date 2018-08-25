
read_css_theme <- function(
  x,
  font_size = getOption("shed.font_size", 14L)
){
  paste(
    paste(readLines(x), collapse = "\n"),
    sprintf("#hot tr td { font-size: %spx;  }", font_size)
  )
}

