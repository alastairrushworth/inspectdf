context("inspect_na single dataframe")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)
# add a logical column
tdf$height_lg <- tdf$height > 160

test_that("Output is a data frame", {
  expect_is(inspect_na(mtcars), "data.frame")
  expect_is(inspect_na(band_instruments), "data.frame")
  expect_is(inspect_na(tdf), "data.frame")
})

