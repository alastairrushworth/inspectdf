context("inspect_na pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)
# add a logical column
starwars$mass_lg <- starwars$mass > 70

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_na(mtcars, mtcars), "data.frame")
  expect_is(inspect_na(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_na(starwars, starwars), "data.frame")
  expect_is(inspect_na(storms, storms), "data.frame")
  expect_is(inspect_na(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_na(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_na(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})