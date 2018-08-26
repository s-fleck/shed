context("sheditor")


test_that("sheditor works as expected", {

  fmt <- shed_format(
    "csv", shed:::shed_read_csv,  shed:::shed_write_csv
  )

  x <- sheditor$new(
    x = iris,
    fname = tempfile(),
    format = fmt
  )

  y <- x$edit( )

})
