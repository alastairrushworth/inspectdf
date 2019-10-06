library(vdiffr)
library(tidyverse)
context("inspect_cor with grouped dataframes")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("inspect_cor & grouped dataframe = df (mtcars)", {
  x <- try(mtcars %>% dplyr::group_by(am) %>% inspect_cor(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "am")
})

test_that("inspect_cor & grouped dataframe = df (storms)", {
  x <- try(storms %>% dplyr::group_by(status) %>% inspect_cor(), silent = TRUE)
  expect_is(x, "data.frame")
  expect_equal(attr(x, "type")$input_type, "grouped")
  expect_equal(colnames(x)[1], "status")
})

test_that("inspect_cor for tech", {
  x_year <- try(tech %>% dplyr::group_by(year) %>% inspect_cor(), silent = TRUE)
  x_quarter <- try(tech %>% dplyr::group_by(quarter) %>% inspect_cor(), silent = TRUE)
  expect_is(x_year, "data.frame")
  expect_is(x_quarter, "data.frame")
  expect_equal(attr(x_year, "type")$input_type, "grouped")
  expect_equal(attr(x_quarter, "type")$input_type, "grouped")
  expect_equal(colnames(x_quarter)[1], "quarter")
  expect_equal(colnames(x_year)[1], "year")
})

test_that('inspect_cor grouped equals equivalent cor call', {
  nr = 100
  set.seed(22)
  df1 = tibble(a = rnorm(nr),
               b = rnorm(nr), 
               group = sample(c('group_1', 'group_2'), nr, r = T))
  z_cor <- df1 %>% 
    group_by(group) %>% 
    summarize(c = cor(a, b))
  z_idf <- df1 %>%
    group_by(group) %>%
    inspect_cor(with_col = "a") %>%
    filter(col_2 == "b")
  expect_equal(z_cor$group, z_idf$group)
  expect_equal(z_cor$c, z_idf$corr)
})