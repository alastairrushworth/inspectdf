context("report_imbalance")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

test_that("Output is a data frame", {
  expect_is(report_imbalance(mtcars), "data.frame")
  expect_is(report_imbalance(band_instruments), "data.frame")
  expect_error(report_imbalance(nasa))
  expect_is(report_imbalance(starwars), "data.frame")
  expect_is(report_imbalance(storms), "data.frame")
  expect_is(report_imbalance(airquality), "data.frame")
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
  expect_is(report_imbalance(mtcars, mtcars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(band_instruments, band_instruments %>% sample_n(100, replace = T)) , "data.frame")
  expect_is(report_imbalance(starwars, starwars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(storms, storms %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_imbalance(airquality, airquality%>% sample_n(100, replace = T)), "data.frame")
})