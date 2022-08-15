context("inspect_types pair of dataframes")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_types(mtcars, mtcars), "data.frame")
  expect_is(inspect_types(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_types(tdf, tdf), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_types(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_types(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_types(tdf, tdf %>% dplyr::sample_n(100, replace = T)), "data.frame")
  })