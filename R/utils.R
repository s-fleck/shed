is_css_file <- function(x){
  is_scalar_character(x) &&
  grepl("\\.css$", x, ignore.case = TRUE)
}
