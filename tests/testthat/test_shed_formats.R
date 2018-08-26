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

  for (fmt in fmts){
    fmt$write_fun(tdat, tf)
    expect_equal(
      tdat,
      prep_input_df(fmt$read_fun(tf, locale = readr::locale()))
    )
  }


})
