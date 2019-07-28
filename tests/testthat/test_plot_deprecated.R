context("show_plot deprecated")

# load in some example data
data("starwars", package = "dplyr")
library(vdiffr)
set.seed(21)

test_that("depracated plots", {
  expect_warning(starwars %>% inspect_cat(show_plot = TRUE))
  expect_warning(starwars %>% inspect_cor(show_plot = TRUE))
  expect_warning(starwars %>% inspect_imb(show_plot = TRUE))
  expect_warning(starwars %>% inspect_mem(show_plot = TRUE))
  expect_warning(starwars %>% inspect_na(show_plot = TRUE))
  expect_warning(starwars %>% inspect_num(show_plot = TRUE))
  expect_warning(starwars %>% inspect_types(show_plot = TRUE))
})


