context("inspect_mem pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_mem(mtcars, mtcars), "data.frame")
  expect_is(inspect_mem(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_mem(starwars, starwars), "data.frame")
  expect_is(inspect_mem(storms, storms), "data.frame")
  expect_is(inspect_mem(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_mem(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_mem(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_mem(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_mem(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_mem(airquality, airquality %>% dplyr::sample_n(100, replace = T)), "data.frame")
})