context("inspect_na single dataframe")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)
# add a logical column
starwars$mass_lg <- starwars$mass > 70

test_that("Output is a data frame", {
  expect_is(inspect_na(mtcars), "data.frame")
  expect_is(inspect_na(band_instruments), "data.frame")
  expect_error(inspect_na(nasa))
  expect_is(inspect_na(starwars), "data.frame")
  expect_is(inspect_na(storms), "data.frame")
  expect_is(inspect_na(airquality), "data.frame")
})

