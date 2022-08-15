context("inspect_imb")

# load in some example data
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_imb(mtcars), "data.frame")
  expect_is(inspect_imb(band_instruments), "data.frame")
  expect_is(inspect_imb(tdf), "data.frame")
  expect_is(inspect_imb(storms), "data.frame")
})

test_that("Check counts for character columns", {
  tdf_char <- tdf %>% dplyr::mutate_if(is.factor, as.character)
  tdf_imb <- tdf_char %>% inspect_imb()
  max_cnts <- rev(sort(sapply(lapply(tdf_char %>% dplyr::select_if(is.character), table), max)))
  expect_equivalent(tdf_imb$cnt, max_cnts)
})

test_that("Check counts for factor columns", {
  tdf_factor <- tdf %>% dplyr::mutate_if(is.character, as.factor)
  tdf_imb <- tdf_factor %>% inspect_imb()
  max_cnts <- rev(sort(sapply(lapply(tdf_factor %>% dplyr::select_if(is.factor), table), max)))
  expect_equivalent(tdf_imb$cnt, max_cnts)
})