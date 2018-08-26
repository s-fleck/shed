sheditor_retval <- function(
  data,
  fname,
  locale
){
  structure(
    list(
      data = data,
      fname = fname
    ),
    class = "sheditor_retval"
  )
}
