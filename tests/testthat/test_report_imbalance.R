context("report_imb")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_imb(mtcars), "data.frame")
  expect_is(report_imb(band_instruments), "data.frame")
  expect_error(report_imb(nasa))
  expect_is(report_imb(starwars), "data.frame")
  expect_is(report_imb(storms), "data.frame")
  expect_is(report_imb(airquality), "data.frame")
})

test_that("Plot is returned without error", {
  expect_is(report_imb(mtcars, show_plot = T), "data.frame")
  expect_is(report_imb(band_instruments, show_plot = T), "data.frame")
  expect_error(report_imb(nasa, show_plot = T))
  expect_is(report_imb(starwars, show_plot = T), "data.frame")
  expect_is(report_imb(storms, show_plot = T), "data.frame")
  expect_is(report_imb(airquality, show_plot = T), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_imb(mtcars, mtcars), "data.frame")
  expect_is(report_imb(band_instruments, band_instruments), "data.frame")
  expect_is(report_imb(starwars, starwars), "data.frame")
  expect_is(report_imb(storms, storms), "data.frame")
  expect_is(report_imb(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_imb(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imb(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_imb(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imb(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_imb(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})