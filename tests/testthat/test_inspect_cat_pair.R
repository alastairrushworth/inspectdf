context("inspect_cat pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)
# exampe df with dates
x = data.frame(dat  = as.Date("01/10/2017", "%d/%m/%Y") + 1:10, 
               ch   = letters[1:10], 
               nm   = 1:10)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_cat(mtcars, mtcars), "data.frame")
  expect_is(inspect_cat(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_cat(starwars, starwars), "data.frame")
  expect_is(inspect_cat(storms, storms), "data.frame")
  expect_is(inspect_cat(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_cat(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cat(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_cat(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cat(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cat(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})