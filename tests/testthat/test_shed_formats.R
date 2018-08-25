context("shed_formats")


test_that("shed_formats works as expected", {

  shed_formats(
    c("csv", "csv2"),
    list(write.csv, write.csv2),
    list(read.csv, read.csv2)
  )


})
