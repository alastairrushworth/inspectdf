context("inspect_cat single dataframe")

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

test_that("Output is a data frame", {
  expect_is(inspect_cat(mtcars), "data.frame")
  expect_is(inspect_cat(band_instruments), "data.frame")
  expect_error(inspect_cat(nasa))
  expect_is(inspect_cat(starwars), "data.frame")
  expect_is(inspect_cat(storms), "data.frame")
  expect_is(inspect_cat(airquality), "data.frame")
  expect_is(inspect_cat(x), "data.frame")
})

test_that("Output with different columns", {
  set.seed(10)
  expect_is(inspect_cat(mtcars %>% dplyr::select(-1), mtcars %>% dplyr::select(-2)), "data.frame")
  expect_is(inspect_cat(band_instruments %>% dplyr::select(-1), band_instruments %>% dplyr::sample_n(100, replace = T)) , "data.frame")
  expect_is(inspect_cat(starwars%>% dplyr::select(-1), starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cat(storms, storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)), "data.frame")
  expect_is(inspect_cat(airquality, airquality %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)), "data.frame")
})

test_that("Majority NA in standard dataframes does not throw error", {
  expect_is(inspect_cat(data.frame(misst = c("a", "b", NA, NA))), "data.frame")
})




