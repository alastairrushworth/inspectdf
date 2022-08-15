library(vdiffr)
context("inspect_num with grouped dataframes")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("inspect_num & grouped dataframe = df (mtcars)", {
  x <- try(mtcars %>% dplyr::group_by(am) %>% inspect_num(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "am")
})

test_that("inspect_num & grouped dataframe = df (storms)", {
  x <- try(storms %>% dplyr::group_by(status) %>% inspect_num(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "status")
})
