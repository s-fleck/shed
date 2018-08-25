context("sheditor")


test_that("sheditor works as expected", {

  fmt <- shed_format(
    "csv", shed:::shed_write_csv, shed:::shed_read_csv
  )

  x <- sheditor$new(
    x = iris,
    fname = "blah/blubb.csv",
    format = fmt
  )

  x$edit( )

})
