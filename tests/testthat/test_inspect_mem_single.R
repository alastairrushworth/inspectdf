context("inspect_mem")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_mem(mtcars), "data.frame")
  expect_is(inspect_mem(band_instruments), "data.frame")
  expect_is(inspect_mem(tdf), "data.frame")
})