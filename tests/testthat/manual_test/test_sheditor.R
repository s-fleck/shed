context("Sheditor")


test_that("Sheditor works as expected", {

  fmt <- shed_format(
    "csv", shed:::shed_read_csv,  shed:::shed_write_csv
  )

  x <- Sheditor$new(
    input = iris,
    file = tempfile(),
    format = fmt
  )

  y <- x$edit( )

})
