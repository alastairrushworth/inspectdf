context("report_association")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_association(mtcars), "data.frame")
  expect_is(report_association(band_instruments), "data.frame")
  expect_error(report_association(nasa))
  expect_is(report_association(starwars), "data.frame")
  expect_is(report_association(storms), "data.frame")
  expect_is(report_association(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_association(mtcars, mtcars), "data.frame")
  expect_is(report_association(band_instruments, band_instruments), "data.frame")
  expect_is(report_association(starwars, starwars), "data.frame")
  expect_is(report_association(storms, storms), "data.frame")
  expect_is(report_association(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_association(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_association(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_association(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_association(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_association(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

