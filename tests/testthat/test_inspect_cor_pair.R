library(vdiffr)
context("inspect_cor with pair dataframes")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_cor(mtcars, mtcars), "data.frame")
  expect_is(inspect_cor(tdf, tdf), "data.frame")
  expect_is(inspect_cor(storms, storms), "data.frame")
  expect_is(inspect_cor(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_cor(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_cor(tdf, tdf %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

