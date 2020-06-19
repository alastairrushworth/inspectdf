context("inspect_imb")

# load in some example data
data("starwars", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_imb(mtcars), "data.frame")
  expect_is(inspect_imb(band_instruments), "data.frame")
  expect_is(inspect_imb(starwars), "data.frame")
  expect_is(inspect_imb(storms), "data.frame")
  expect_is(inspect_imb(airquality), "data.frame")
  expect_is(inspect_imb(iris), "data.frame")
})

test_that("Check counts for character columns", {
  starwars_char <- starwars %>% dplyr::mutate_if(is.factor, as.character)
  starwars_imb <- starwars_char %>% inspect_imb()
  max_cnts <- rev(sort(sapply(lapply(starwars_char %>% dplyr::select_if(is.character), table), max)))
  expect_equivalent(starwars_imb$cnt, max_cnts)
})

test_that("Check counts for factor columns", {
  starwars_factor <- starwars %>% dplyr::mutate_if(is.character, as.factor)
  starwars_imb <- starwars_factor %>% inspect_imb()
  max_cnts <- rev(sort(sapply(lapply(starwars_factor %>% dplyr::select_if(is.factor), table), max)))
  expect_equivalent(starwars_imb$cnt, max_cnts)
})