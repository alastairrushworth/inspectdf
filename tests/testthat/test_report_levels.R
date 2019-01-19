context("report_cat")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_cat(mtcars), "data.frame")
  expect_is(report_cat(band_instruments), "data.frame")
  expect_error(report_cat(nasa))
  expect_is(report_cat(starwars), "data.frame")
  expect_is(report_cat(storms), "data.frame")
  expect_is(report_cat(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_cat(mtcars, mtcars), "data.frame")
  expect_is(report_cat(band_instruments, band_instruments), "data.frame")
  expect_is(report_cat(starwars, starwars), "data.frame")
  expect_is(report_cat(storms, storms), "data.frame")
  expect_is(report_cat(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_cat(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cat(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_cat(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cat(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cat(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})