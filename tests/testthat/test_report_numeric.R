context("report_numeric")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_numeric(mtcars), "data.frame")
  expect_is(report_numeric(band_instruments), "data.frame")
  expect_error(report_numeric(nasa))
  expect_is(report_numeric(starwars), "data.frame")
  expect_is(report_numeric(storms), "data.frame")
  expect_is(report_numeric(airquality), "data.frame")
})

test_that("Plotting works ok", {
  expect_is(report_numeric(mtcars, show_plot = T), "data.frame")
  expect_is(report_numeric(band_instruments, show_plot = T), "data.frame")
  expect_is(report_numeric(starwars, show_plot = T), "data.frame")
  expect_is(report_numeric(storms, show_plot = T), "data.frame")
  expect_is(report_numeric(airquality, show_plot = T), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_numeric(mtcars, mtcars), "data.frame")
  expect_is(report_numeric(band_instruments, band_instruments), "data.frame")
  expect_is(report_numeric(starwars, starwars), "data.frame")
  expect_is(report_numeric(storms, storms), "data.frame")
  expect_is(report_numeric(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_numeric(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_numeric(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})