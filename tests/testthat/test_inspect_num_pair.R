context("inspect_num pair of dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_num(mtcars, mtcars), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_num(starwars, starwars), "data.frame")
  expect_is(inspect_num(storms, storms), "data.frame")
  expect_is(inspect_num(airquality, airquality), "data.frame")
})

test_that("Output where columns are missing from either df", {
  set.seed(10)
  star_1 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-birth_year)
  star_2 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-1, -2)
  expect_is(inspect_num(star_1, star_2), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_num(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_num(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Output with two different ranges data frame", {
  set.seed(10)
  expect_is(inspect_num(mtcars, mtcars %>% dplyr::mutate_if(is.numeric, ~. + 5)), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments %>% dplyr::mutate_if(is.numeric, ~. - 5)) , "data.frame")
  expect_is(inspect_num(starwars, starwars %>% dplyr::mutate_if(is.numeric, ~. + 5)), "data.frame")
  expect_is(inspect_num(storms, storms %>% dplyr::mutate_if(is.numeric, ~. - 5)), "data.frame")
  expect_is(inspect_num(airquality, airquality %>% dplyr::mutate_if(is.numeric, ~. + 5)), "data.frame")
})

test_that("Output with two different ranges and different column sets", {
  set.seed(10)
  expect_is(inspect_num(mtcars[,1:6], mtcars[,3:11] %>% dplyr::mutate_if(is.numeric, ~. + 5)), "data.frame")
  expect_is(inspect_num(starwars %>% select(-height), 
                        starwars %>% select(-mass) %>% dplyr::mutate_if(is.numeric, ~. + 5)), "data.frame")
})
