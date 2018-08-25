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
