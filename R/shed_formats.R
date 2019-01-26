# ShedFormat --------------------------------------------------------------
#' @include utils-sfmisc.R
#'
ShedFormat <-  R6::R6Class(
  "ShedFormat",
  public = list(
    initialize = function(
      read_fun,
      write_fun
    ){
      stopifnot(
        is_read_fun(read_fun),
        is_write_fun(write_fun)
      )
      self$read  <- read_fun
      self$write <- write_fun
    },
    read  = NULL,
    write = NULL
  )
)




is_ShedFormat <- function(x){
  inherits(x, "ShedFormat")
}




# rw funs -----------------------------------------------------------------

shed_read_csv   <- function(
  path,
  locale
){
  lg$debug("Reading csv file", file = path, encoding = locale$encoding)

  res <- as.data.frame(
    readr::read_csv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c")),
    locale = locale
  )

  mostattributes(res) <- NULL
  has_colnames_row(res) <- TRUE
  res
}




shed_read_csv2  <- function(
  path,
  locale
){
  lg$debug("Reading csv file", file = path, encoding = locale$encoding)

  res <- suppressMessages(as.data.frame(
    readr::read_csv2(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      locale = locale
    )
  ))

  mostattributes(res) <- NULL
  has_colnames_row(res) <- TRUE
  res
}




shed_read_tsv  <- function(
  path,
  locale
){
  lg$debug("Reading tsv file", file = path, encoding = locale$encoding)

  res <- suppressMessages(as.data.frame(
    readr::read_tsv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      locale = locale
    )
  ))

  mostattributes(res) <- NULL
  has_colnames_row(res) <- TRUE
  res
}




shed_write_csv  <- function(x, path){
  readr::write_csv(x, path, col_names = FALSE, na = "")
}




shed_write_csv2 <- function(x, path){
  readr::write_csv2(x, path, col_names = FALSE, na = "")
}




shed_write_excel_csv  <- function(x, path){
  readr::write_excel_csv(x, path, col_names = FALSE, na = "")
}




shed_write_excel_csv2 <- function(x, path){
  readr::write_excel_csv2(x, path, col_names = FALSE, na = "")
}




shed_write_tsv <- function(x, path) {
  readr::write_tsv(x, path, col_names = FALSE, na = "")
}




# predicates --------------------------------------------------------------

is_read_fun <- function(x){
  is.function(x) &&
    identical(names(formals(x)), c("path", "locale"))
}




is_write_fun <- function(x){
  is.function(x) &&
    identical(names(formals(x)), c("x", "path"))
}




# formats -----------------------------------------------------------------

shed_format_csv   <- ShedFormat$new(shed_read_csv, shed_write_csv)
shed_format_csv2  <- ShedFormat$new(shed_read_csv2, shed_write_csv2)
shed_format_csvx  <- ShedFormat$new(shed_read_csv, shed_write_excel_csv)
shed_format_csv2x <- ShedFormat$new(shed_read_csv2, shed_write_excel_csv2)
