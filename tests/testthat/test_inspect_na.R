context("inspect_na")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_na(mtcars), "data.frame")
  expect_is(inspect_na(band_instruments), "data.frame")
  expect_error(inspect_na(nasa))
  expect_is(inspect_na(starwars), "data.frame")
  expect_is(inspect_na(storms), "data.frame")
  expect_is(inspect_na(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_na(mtcars, mtcars), "data.frame")
  expect_is(inspect_na(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_na(starwars, starwars), "data.frame")
  expect_is(inspect_na(storms, storms), "data.frame")
  expect_is(inspect_na(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_na(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_na(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_na(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})
test_that("Proportion test is correct", {
  d1 <- starwars %>% select(birth_year)
  d2 <- starwars[1:20, ] %>% select(birth_year)
  p1 <- inspect_na(d1, d2) %>% select(6) %>% as.numeric
  p2 <- prop.test(c(sum(is.na(d1)), sum(is.na(d2))), c(nrow(d1), nrow(d2)))$p.value
  expect_equal(p1, p2)
})

