context("inspect_num")

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

test_that("Plotting works ok", {
  expect_is(inspect_num(mtcars, show_plot = T), "data.frame")
  expect_is(inspect_num(band_instruments, show_plot = T), "data.frame")
  expect_is(inspect_num(starwars, show_plot = T), "data.frame")
  expect_is(inspect_num(storms, show_plot = T), "data.frame")
  expect_is(inspect_num(airquality, show_plot = T), "data.frame")
})

test_that("Multiple plot layouts", {
  expect_is(inspect_num(storms, show_plot = T, plot_layout = c(4, 3)), "data.frame")
  expect_is(inspect_num(storms, storms, show_plot = T, plot_layout = c(4, 3)), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_num(mtcars, mtcars), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments), "data.frame")
  expect_is(inspect_num(starwars, starwars), "data.frame")
  expect_is(inspect_num(storms, storms), "data.frame")
  expect_is(inspect_num(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_num(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_num(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_num(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Output where columns are missing from either df", {
  set.seed(10)
  star_1 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-birth_year)
  star_2 <- starwars %>% dplyr::sample_n(50) %>% dplyr::select(-1, -2)
  expect_is(inspect_num(star_1, star_2, show_plot = TRUE), "data.frame")
})

