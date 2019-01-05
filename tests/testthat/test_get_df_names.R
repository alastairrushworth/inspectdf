context("get_df_names")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("df names can be recovered from pipe", {
  expect_identical(mtcars %>% get_df_names_test(storms), list(df1 = "mtcars", df2 = "storms"))
})

test_that("df names can be recovered from long pipe", {
  expect_identical(mtcars %>% select(1:3) %>% get_df_names_test(storms), list(df1 = "mtcars", df2 = "storms"))
})

test_that("df names are recovered without pipe", {
  expect_identical(get_df_names_test(mtcars, storms), list(df1 = "mtcars", df2 = "storms"))
})

test_that("df names are not allowed to be identical", {
  expect_identical(get_df_names_test(mtcars, mtcars), list(df1 = "mtcars", df2 = "mtcars_2"))
})