shed_formats <- function(
  name,
  read_fun,
  write_fun
){
  stopifnot(
    is.character(name),
    is.list(read_fun),
    is.list(write_fun)
  )

  res <- mapply(
    function(n, r, w) shed_format(n, r, w),
    name,
    read_fun,
    write_fun,
    SIMPLIFY = FALSE
  )

  res <- do.call(rbind, res)
  class(res) <- union("shed_formats", class(res))
  res
}



# rw funs -----------------------------------------------------------------

shed_read_csv   <- function(
  path,
  locale
){
  flog.debug("Reading file %s with encoding %s", path, locale$encoding)

  res <- as.data.frame(
    readr::read_csv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c")),
    locale = locale
  )

  mostattributes(res) <- NULL
  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
  res
}




shed_read_csv2  <- function(
  path,
  locale
){
  flog.debug("Reading file %s with encoding %s", path, locale$encoding)

  res <- suppressMessages(as.data.frame(
    readr::read_csv2(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      locale = locale
    )
  ))

  mostattributes(res) <- NULL

  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
  res
}




shed_read_tsv  <- function(
  path,
  locale
){
  flog.debug("Reading tsv file %s with encoding %s", path, locale$encoding)


  res <- suppressMessages(as.data.frame(
    readr::read_tsv(
      path,
      col_names = FALSE,
      col_types = readr::cols(.default = "c"),
      locale = locale
    )
  ))

  mostattributes(res) <- NULL
  flog.trace("Loaded data.frame: \n%s", to_string(head(res)))
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
  print(x)
  print(path)
  readr::write_excel_csv2(x, path, col_names = FALSE, na = "")
}




shed_write_tsv <- function(x, path) {
  readr::write_tsv(x, path, col_names = FALSE, na = "")
}



# formats -----------------------------------------------------------------

shed_format <- function(
  name,
  read_fun,
  write_fun
){
  stopifnot(
    is.character(name),
    is.function(read_fun),
    is.function(write_fun)
  )

  tibble::new_tibble(
    list(
      name = name,
      read_fun = list(read_fun),
      write_fun = list(write_fun)
    ),
    subclass = "shed_format"
  )
}


shed_format_csv   <- shed_format("csv",   shed_read_csv, shed_write_csv)
shed_format_csv2  <- shed_format("csv2",  shed_read_csv2, shed_write_csv)
shed_format_csvx  <- shed_format("csvx",  shed_read_csv, shed_write_excel_csv)
shed_format_csv2x <- shed_format("csv2x", shed_read_csv2, shed_write_excel_csv)





# helpers -----------------------------------------------------------------

is_read_fun <- function(x){
  is.function(x) &&
  identical(names(formals(x)), c("path", "locale"))
}


is_write_fun <- function(x){
  is.function(x) &&
  identical(names(formals(x)), c("x", "path"))
}
