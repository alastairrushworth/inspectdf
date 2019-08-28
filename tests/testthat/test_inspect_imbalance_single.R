context("inspect_imb")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_imb(mtcars), "data.frame")
  expect_is(inspect_imb(band_instruments), "data.frame")
  expect_error(inspect_imb(nasa))
  expect_is(inspect_imb(starwars), "data.frame")
  expect_is(inspect_imb(storms), "data.frame")
  expect_is(inspect_imb(airquality), "data.frame")
  expect_is(inspect_imb(iris), "data.frame")
})

