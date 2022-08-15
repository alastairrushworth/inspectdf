context("inspect_num pair of dataframes")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_num(mtcars, mtcars), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_num(tdf, tdf), "data.frame")
})

test_that("Output where columns are missing from either df", {
  set.seed(10)
  tdf_1 <- tdf %>% dplyr::sample_n(50) %>% dplyr::select(-stage_wins)
  tdf_2 <- tdf %>% dplyr::sample_n(50) %>% dplyr::select(-height)
  expect_is(inspect_num(tdf_1, tdf_2), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_num(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_num(tdf, tdf %>% dplyr::sample_n(100, replace = T)), "data.frame")
  })