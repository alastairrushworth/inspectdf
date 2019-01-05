context("report_space")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_space(mtcars), "data.frame")
  expect_is(report_space(band_instruments), "data.frame")
  expect_error(report_space(nasa))
  expect_is(report_space(starwars), "data.frame")
  expect_is(report_space(storms), "data.frame")
  expect_is(report_space(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_space(mtcars, mtcars), "data.frame")
  expect_is(report_space(band_instruments, band_instruments), "data.frame")
  expect_is(report_space(starwars, starwars), "data.frame")
  expect_is(report_space(storms, storms), "data.frame")
  expect_is(report_space(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_space(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_space(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_space(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_space(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_space(airquality, airquality %>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Plot output works", {
  set.seed(10)
  expect_is(report_space(mtcars, mtcars %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_space(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T), show_plot = T) , "data.frame")
  expect_is(report_space(starwars, starwars %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_space(storms, storms %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_space(airquality, airquality %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
})