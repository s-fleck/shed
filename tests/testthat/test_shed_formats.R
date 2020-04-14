context("shed_formats")


test_that("shed_formats works as expected", {

  tf <- tempfile()

  fmts <- list(
    shed_format_csv,
    shed_format_csvx,
    shed_format_csv2x,
    shed_format_csv2
  )

  tdat <- prep_input_df(iris)
  tres <- iris
  tres$Species <- as.character(tres$Species)

  for (fmt in fmts){
    fmt$write(colnames_to_row(tdat), tf)
    expect_equal(
      tres,
      parse_output_df(fmt$read(tf, locale = readr::locale())),
      check.attributes = FALSE
    )
  }
})





test_that("csvy reading and writing works as expected", {

  res <- shed_read_csvy(testthis::find_testdata("iris.csvy"))
  shed2y(testthis::find_testdata("iris.csvy"))

})
