context("inspect_cor")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_cor(mtcars), "data.frame")
  expect_is(inspect_cor(band_instruments), "data.frame")
  expect_error(inspect_cor(nasa))
  expect_is(inspect_cor(starwars), "data.frame")
  expect_is(inspect_cor(storms), "data.frame")
  expect_is(inspect_cor(airquality), "data.frame")
})

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

diff_correlatations <- function(data_input){
  x1 <- data_input %>% inspect_cor %>% dplyr::select(corr)
  x <- data_input %>% dplyr::select_if(is.numeric)
  x <- cor(x, use = "pairwise.complete.obs")
  diag(x) <- NA
  x2 <- suppressWarnings(x %>% c %>% tibble::as_tibble() %>% 
    dplyr::rename(corr = value) %>% dplyr::filter(!is.na(corr)) %>%
    dplyr::distinct() %>% dplyr::arrange(dplyr::desc(abs(corr))))
  return(mean(unlist(abs(x1 - x2))))
}

test_that("Output correlations are correct", {
  expect_lt(diff_correlatations(mtcars), 10^-15)
  expect_lt(diff_correlatations(starwars), 10^-15)
  expect_lt(diff_correlatations(storms), 10^-15)
  expect_lt(diff_correlatations(airquality), 10^-15)
})

test_that("Single column returns empty df", {
  expect_equal(nrow(inspect_cor(mtcars %>% select(1))), 0)
})

test_that("Constant columns return NA", {
  expect_warning(inspect_cor(data.frame(z = 1:10, y = rep(1, 10))))
  expect_equal(suppressWarnings(sum(is.na(inspect_cor(data.frame(z = 1:10, y = rep(1, 10)))$corr))), 1)
})










