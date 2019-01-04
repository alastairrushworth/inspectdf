context("report_numeric")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

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
  expect_is(report_numeric(mtcars, mtcars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(band_instruments, band_instruments %>% sample_n(100, replace = T)) , "data.frame")
  expect_is(report_numeric(starwars, starwars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(storms, storms %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_numeric(airquality, airquality%>% sample_n(100, replace = T)), "data.frame")
})