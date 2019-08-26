context("inspect_num single dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_num(mtcars), "data.frame")
  expect_is(inspect_num(band_instruments), "data.frame")
  expect_error(inspect_num(nasa))
  expect_is(inspect_num(starwars), "data.frame")
  expect_is(inspect_num(storms), "data.frame")
  expect_is(inspect_num(airquality), "data.frame")
})

test_that("Output where columns are missing from either df", {
  set.seed(10)
  star_1 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-birth_year)
  star_2 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-1, -2)
  expect_is(inspect_num(star_1, star_2), "data.frame")
})

test_that("Number of breaks", {
  expect_is(iris %>% inspect_num(breaks = 30), "data.frame")
  expect_is(iris %>% inspect_num(breaks = 10), "data.frame")
})

test_that("Cope with columns with missing values", {
  with_missing <- data.frame(a = 1:100, b = rep(NA_real_, 100)) %>% inspect_num
  expect_equal(with_missing$pcnt_na[2], 100)
  expect_equal(with_missing$hist$b$value, NA)
})