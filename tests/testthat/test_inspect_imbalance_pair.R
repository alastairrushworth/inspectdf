context("inspect_imb pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_imb(mtcars, mtcars), "data.frame")
  expect_is(inspect_imb(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_imb(starwars, starwars), "data.frame")
  expect_is(inspect_imb(storms, storms), "data.frame")
  expect_is(inspect_imb(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_imb(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_imb(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_imb(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_imb(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_imb(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})