context("report_mem")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_mem(mtcars), "data.frame")
  expect_is(report_mem(band_instruments), "data.frame")
  expect_error(report_mem(nasa))
  expect_is(report_mem(starwars), "data.frame")
  expect_is(report_mem(storms), "data.frame")
  expect_is(report_mem(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_mem(mtcars, mtcars), "data.frame")
  expect_is(report_mem(band_instruments, band_instruments), "data.frame")
  expect_is(report_mem(starwars, starwars), "data.frame")
  expect_is(report_mem(storms, storms), "data.frame")
  expect_is(report_mem(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_mem(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_mem(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_mem(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_mem(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_mem(airquality, airquality %>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Plot output works", {
  set.seed(10)
  expect_is(report_mem(mtcars, mtcars %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_mem(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T), show_plot = T) , "data.frame")
  expect_is(report_mem(starwars, starwars %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_mem(storms, storms %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
  expect_is(report_mem(airquality, airquality %>% dplyr::sample_n(100, replace = T), show_plot = T), "data.frame")
})