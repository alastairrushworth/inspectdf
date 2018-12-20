context("report_cor")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

test_that("Output is a data frame", {
  expect_is(report_cor(mtcars), "data.frame")
  expect_is(report_cor(band_instruments), "data.frame")
  expect_error(report_cor(nasa))
  expect_is(report_cor(starwars), "data.frame")
  expect_is(report_cor(storms), "data.frame")
  expect_is(report_cor(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_cor(mtcars, mtcars), "data.frame")
  expect_is(report_cor(band_instruments, band_instruments), "data.frame")
  expect_error(report_cor(nasa, nasa))
  expect_is(report_cor(starwars, starwars), "data.frame")
  expect_is(report_cor(storms, storms), "data.frame")
  expect_is(report_cor(airquality, airquality), "data.frame")
})
