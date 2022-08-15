context("inspect_types single dataframe")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_types(mtcars), "data.frame")
  expect_is(inspect_types(band_instruments), "data.frame")
  expect_is(inspect_types(tdf), "data.frame")
})