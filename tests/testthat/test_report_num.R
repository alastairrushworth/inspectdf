context("report_num")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_num(mtcars), "data.frame")
  expect_is(report_num(band_instruments), "data.frame")
  expect_error(report_num(nasa))
  expect_is(report_num(starwars), "data.frame")
  expect_is(report_num(storms), "data.frame")
  expect_is(report_num(airquality), "data.frame")
})

test_that("Plotting works ok", {
  expect_is(report_num(mtcars, show_plot = T), "data.frame")
  expect_is(report_num(band_instruments, show_plot = T), "data.frame")
  expect_is(report_num(starwars, show_plot = T), "data.frame")
  expect_is(report_num(storms, show_plot = T), "data.frame")
  expect_is(report_num(airquality, show_plot = T), "data.frame")
})

test_that("Multiple plot layouts", {
  expect_is(report_num(storms, show_plot = T, plot_layout = c(4, 3)), "data.frame")
  expect_is(report_num(storms, storms, show_plot = T, plot_layout = c(4, 3)), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_num(mtcars, mtcars), "data.frame")
  expect_is(report_num(band_instruments, band_instruments), "data.frame")
  expect_is(report_num(starwars, starwars), "data.frame")
  expect_is(report_num(storms, storms), "data.frame")
  expect_is(report_num(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_num(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_num(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_num(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_num(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_num(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})