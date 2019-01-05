context("report_levels")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_levels(mtcars), "data.frame")
  expect_is(report_levels(band_instruments), "data.frame")
  expect_error(report_levels(nasa))
  expect_is(report_levels(starwars), "data.frame")
  expect_is(report_levels(storms), "data.frame")
  expect_is(report_levels(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_levels(mtcars, mtcars), "data.frame")
  expect_is(report_levels(band_instruments, band_instruments), "data.frame")
  expect_is(report_levels(starwars, starwars), "data.frame")
  expect_is(report_levels(storms, storms), "data.frame")
  expect_is(report_levels(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_levels(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_levels(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_levels(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_levels(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_levels(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})