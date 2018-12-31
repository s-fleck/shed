context("shed")


test_that("shed works as expected", {

  tdat <- data.frame(
    Sepal.Length = c("Sepal.Length", "blah", "4.9", "4.7"),
    Sepal.Width = c("Sepal.Width", "3.5", "3.0", "3.2"),
    Petal.Length = c("Petal.Length", "1.4", "1.4", "1.3"),
    Petal.Width = c("Petal.Width", "0.2", "0.2", "0.2"),
    Species = c("Species", "setosa", "setosa", "setosa"),
    stringsAsFactors = FALSE
  )

  tres <- parse_output_df(tdat)

  expect_identical(
    names(tres),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_true(is.character(tres[[1]]))
  expect_true(is.numeric(tres[[2]]))
  expect_true(is.numeric(tres[[3]]))
  expect_true(is.numeric(tres[[4]]))
  expect_true(is.character(tres[[5]]))


  tdat2 <- data.frame(
    X1 = c("X1", ""),
    stringsAsFactors = FALSE
  )


  tres <- parse_output_df(tdat2)

  expect_equal(
    tres,
    data.frame(
      X1 = NA,
      stringsAsFactors = FALSE
    )
  )
})
