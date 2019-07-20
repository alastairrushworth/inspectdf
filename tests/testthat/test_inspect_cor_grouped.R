library(vdiffr)
context("inspect_cor with grouped dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("inspect_cor & grouped dataframe = df (mtcars)", {
  x <- try(mtcars %>% dplyr::group_by(am) %>% inspect_cor(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "am")
})

test_that("inspect_cor & grouped dataframe = df (storms)", {
  x <- try(storms %>% dplyr::group_by(status) %>% inspect_cor(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "status")
})

test_that("inspect_cor for tech", {
  x_year <- try(tech %>% dplyr::group_by(year) %>% inspect_cor(), silent = TRUE)
  x_quarter <- try(tech %>% dplyr::group_by(quarter) %>% inspect_cor(), silent = TRUE)
  expect_is(x_year, "data.frame")
  expect_is(x_quarter, "data.frame")
  expect_equal(attr(x_year, "type")$input_type, "grouped")
  expect_equal(attr(x_quarter, "type")$input_type, "grouped")
  expect_equal(colnames(x_quarter)[1], "quarter")
  expect_equal(colnames(x_year)[1], "year")
})

test_that("Grouped df correlation plots work", {
  expect_doppelganger("Inspect-cor-grouped-tech-year", tech %>% dplyr::group_by(year) %>% inspect_cor() %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-tech-quarter", tech %>% dplyr::group_by(quarter) %>% inspect_cor() %>% show_plot)
})