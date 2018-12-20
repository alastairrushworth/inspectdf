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
  expect_error(report_na(nasa, nasa))
  expect_is(report_na(starwars, starwars), "data.frame")
  expect_is(report_na(storms, storms), "data.frame")
  expect_is(report_na(airquality, airquality), "data.frame")
})
