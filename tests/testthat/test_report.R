context("report")

# load in some example data
data(mtcars, band_instruments, nasa, starwars, storms, airquality)

test_that("Output is a data frame", {
  expect_is(report(mtcars), "list")
  expect_is(report(band_instruments), "list")
  expect_error(report(nasa))
  expect_is(report(starwars), "list")
  expect_is(report(storms), "list")
  expect_is(report(airquality), "list")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report(mtcars, mtcars), "list")
  expect_is(report(band_instruments, band_instruments), "list")
  expect_is(report(starwars, starwars), "list")
  expect_is(report(storms, storms), "list")
  expect_is(report(airquality, airquality), "list")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report(mtcars, mtcars %>% sample_n(100, replace = T)), "list")
  expect_is(report(band_instruments, band_instruments %>% sample_n(100, replace = T)) , "list")
  expect_is(report(starwars, starwars %>% sample_n(100, replace = T)), "list")
  expect_is(report(storms, storms %>% sample_n(100, replace = T)), "list")
  expect_is(report(airquality, airquality%>% sample_n(100, replace = T)), "list")
})
