context("report_cor")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(report_cor(mtcars), "data.frame")
  expect_is(report_cor(band_instruments), "data.frame")
  expect_error(report_cor(nasa))
  expect_is(report_cor(starwars), "data.frame")
  expect_is(report_cor(storms), "data.frame")
  expect_is(report_cor(airquality), "data.frame")
})

test_that("Output with two identical df inputs data frame", {
  expect_is(report_cor(mtcars, mtcars), "data.frame")
  expect_is(report_cor(band_instruments, band_instruments), "data.frame")
  expect_is(report_cor(starwars, starwars), "data.frame")
  expect_is(report_cor(storms, storms), "data.frame")
  expect_is(report_cor(airquality, airquality), "data.frame")
})

test_that("Output with two different inputs data frame", {
  set.seed(10)
  expect_is(report_cor(mtcars, mtcars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cor(band_instruments, band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(report_cor(starwars, starwars %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cor(storms, storms %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(report_cor(airquality, airquality%>% dplyr::sample_n(100, replace = T)), "data.frame")
})

diff_correlatations <- function(data_input){
  x1 <- data_input %>% report_cor %>% dplyr::select(correlation)
  x <- data_input %>% dplyr::select_if(is.numeric)
  x <- cor(x, use = "pairwise.complete.obs")
  diag(x) <- NA
  x2 <- x %>% c %>% tibble::as_tibble() %>% 
    dplyr::rename(correlation = value) %>% dplyr::filter(!is.na(correlation)) %>%
    dplyr::distinct() %>% dplyr::arrange(dplyr::desc(abs(correlation)))
  return(mean(unlist(abs(x1 - x2))))
}

test_that("Output correlations are correct", {
  expect_lt(diff_correlatations(mtcars), 10^-15)
  expect_lt(diff_correlatations(starwars), 10^-15)
  expect_lt(diff_correlatations(storms), 10^-15)
  expect_lt(diff_correlatations(airquality), 10^-15)
})















