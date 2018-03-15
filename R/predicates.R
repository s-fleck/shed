is_scalar <- function(x){
  identical(length(x), 1L)
}




is_integerish <- function(x){
  if (!is.numeric(x)) {
    return(FALSE)
  } else {
    is.atomic(x) && all(as.integer(x) == x)
  }
}




is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




is_scalar_character <- function(x){
  is_scalar(x) && is.character(x)
}




is_css_file <- function(x){
  is_scalar_character(x) &&
  grepl("\\.css$", x, ignore.case = TRUE)
}
