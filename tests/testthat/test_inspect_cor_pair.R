library(vdiffr)
context("inspect_cor with pair dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output with two identical df inputs data frame", {
  expect_is(inspect_cor(mtcars, mtcars), "data.frame")
  expect_is(inspect_cor(starwars, starwars), "data.frame")
  expect_is(inspect_cor(storms, storms), "data.frame")
  expect_is(inspect_cor(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(inspect_cor(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_cor(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cor(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Pair comparison correlation plots work", {
  set.seed(1)
  expect_doppelganger("Inspect-cor-starwars2", 
                      starwars %>% 
                        inspect_cor(
                          starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                        ) %>% show_plot)
  expect_doppelganger("Inspect-cor-storms2",   storms %>% 
                        inspect_cor(
                          storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                        ) %>% show_plot)
})
