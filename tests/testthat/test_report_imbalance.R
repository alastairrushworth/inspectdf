context("report_imbalance")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_imbalance(mtcars), "data.frame")
  expect_is(report_imbalance(band_instruments), "data.frame")
  expect_error(report_imbalance(nasa))
  expect_is(report_imbalance(starwars), "data.frame")
  expect_is(report_imbalance(storms), "data.frame")
  expect_is(report_imbalance(airquality), "data.frame")
})

test_that("Plot is returned without error", {
  expect_is(report_imbalance(mtcars, show_plot = T), "data.frame")
  expect_is(report_imbalance(band_instruments, show_plot = T), "data.frame")
  expect_error(report_imbalance(nasa, show_plot = T))
  expect_is(report_imbalance(starwars, show_plot = T), "data.frame")
  expect_is(report_imbalance(storms, show_plot = T), "data.frame")
  expect_is(report_imbalance(airquality, show_plot = T), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_imbalance(mtcars, mtcars), "data.frame")
  expect_is(report_imbalance(band_instruments, band_instruments), "data.frame")
  expect_is(report_imbalance(starwars, starwars), "data.frame")
  expect_is(report_imbalance(storms, storms), "data.frame")
  expect_is(report_imbalance(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_imbalance(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_imbalance(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})