write_csv2 <- function(x, path, na = "NA", append = FALSE, col_names = !append) {
  x <- change_decimal_separator(x, decimal_mark = ",")
  readr::write_delim(x, path, delim = ";", na = na, append = append, col_names = col_names)
}




change_decimal_separator <- function(x, decimal_mark = ",") {
  stopifnot(is.data.frame(x))
  numeric_cols <- vapply(x, is.numeric, logical(1))
  x[numeric_cols] <- lapply(x[numeric_cols], format, decimal.mark = decimal_mark)
  x
}
