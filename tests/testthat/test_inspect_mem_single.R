context("inspect_mem")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_mem(mtcars), "data.frame")
  expect_is(inspect_mem(band_instruments), "data.frame")
  expect_error(inspect_mem(nasa))
  expect_is(inspect_mem(starwars), "data.frame")
  expect_is(inspect_mem(storms), "data.frame")
  expect_is(inspect_mem(airquality), "data.frame")
})