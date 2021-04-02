context("show_plot inspect_types")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_types plots", {
  expect_doppelganger("Inspect-types-starwars", starwars %>% inspect_types %>% show_plot)
  expect_doppelganger("Inspect-types-storms",   storms %>%   inspect_types %>% show_plot)
  expect_doppelganger("Inspect-types-tech",   tech %>%   inspect_types %>% show_plot)
})

test_that("inspect_types plot suppress labels", {
  expect_doppelganger("Inspect-types-no-lab-starwars", starwars %>% inspect_types %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-types-no-lab-tech", tech %>% inspect_types %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-types-no-lab-storms",   storms %>%   inspect_types %>% show_plot(text_labels = FALSE))
})

test_that("inspect_types plot paired", {
  expect_doppelganger("Inspect-types-paired-starwars", starwars %>% inspect_types(x2) %>% show_plot)
  expect_doppelganger("Inspect-types-paired-storms",   storms %>%   inspect_types(y2) %>% show_plot)
})



