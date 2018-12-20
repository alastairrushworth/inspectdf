context("report_types")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

test_that("Output is a data frame", {
  expect_is(report_types(mtcars), "data.frame")
  expect_is(report_types(band_instruments), "data.frame")
  expect_error(report_types(nasa))
  expect_is(report_types(starwars), "data.frame")
  expect_is(report_types(storms), "data.frame")
  expect_is(report_types(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_types(mtcars, mtcars), "data.frame")
  expect_is(report_types(band_instruments, band_instruments), "data.frame")
  expect_error(report_types(nasa, nasa))
  expect_is(report_types(starwars, starwars), "data.frame")
  expect_is(report_types(storms, storms), "data.frame")
  expect_is(report_types(airquality, airquality), "data.frame")
})
