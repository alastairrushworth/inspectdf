context("inspect_num single dataframes")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_num(mtcars), "data.frame")
  expect_is(inspect_num(band_instruments), "data.frame")
  expect_is(inspect_num(tdf), "data.frame")
  expect_is(inspect_num(storms), "data.frame")
  expect_is(inspect_num(airquality), "data.frame")
})

test_that("Number of breaks", {
  expect_is(iris %>% inspect_num(breaks = 30), "data.frame")
  expect_is(iris %>% inspect_num(breaks = 10), "data.frame")
})

test_that("Cope with columns with missing values", {
  with_missing <- data.frame(a = 1:100, b = rep(NA_real_, 100)) %>% inspect_num
  expect_equal(with_missing$pcnt_na[2], 100)
  expect_equal(with_missing$hist$b$value, NA)
})