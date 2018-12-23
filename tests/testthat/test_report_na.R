context("report_na")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

test_that("Output is a data frame", {
  expect_is(report_na(mtcars), "data.frame")
  expect_is(report_na(band_instruments), "data.frame")
  expect_error(report_na(nasa))
  expect_is(report_na(starwars), "data.frame")
  expect_is(report_na(storms), "data.frame")
  expect_is(report_na(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_na(mtcars, mtcars), "data.frame")
  expect_is(report_na(band_instruments, band_instruments), "data.frame")
  expect_is(report_na(starwars, starwars), "data.frame")
  expect_is(report_na(storms, storms), "data.frame")
  expect_is(report_na(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_na(mtcars, mtcars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_na(band_instruments, band_instruments %>% sample_n(100, replace = T)) , "data.frame")
  expect_is(report_na(starwars, starwars %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_na(storms, storms %>% sample_n(100, replace = T)), "data.frame")
  expect_is(report_na(airquality, airquality%>% sample_n(100, replace = T)), "data.frame")
})