context("inspect_types pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_types(mtcars, mtcars), "data.frame")
  expect_is(inspect_types(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_types(starwars, starwars), "data.frame")
  expect_is(inspect_types(storms, storms), "data.frame")
  expect_is(inspect_types(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_types(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_types(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_types(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_types(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_types(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})