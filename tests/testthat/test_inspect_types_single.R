context("inspect_types single dataframe")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_types(mtcars), "data.frame")
  expect_is(inspect_types(band_instruments), "data.frame")
  expect_error(inspect_types(nasa))
  expect_is(inspect_types(starwars), "data.frame")
  expect_is(inspect_types(storms), "data.frame")
  expect_is(inspect_types(airquality), "data.frame")
})