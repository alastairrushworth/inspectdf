context("show_plot inspect_cat")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
library(vdiffr)

test_that("inspect_cat plot basic", {
  expect_doppelganger("Inspect-cat-starwars", starwars %>% inspect_cat %>% show_plot)
  expect_doppelganger("Inspect-cat-tech", tech %>% inspect_cat %>% show_plot)
  expect_doppelganger("Inspect-cat-storms",   storms %>%   inspect_cat %>% show_plot)
})
  
test_that("inspect_cat plot cardinality", {
  expect_doppelganger("Inspect-cat-card-starwars", starwars %>% inspect_cat %>% show_plot(high_cardinality = 1))
  expect_doppelganger("Inspect-cat-card-tech", tech %>% inspect_cat %>% show_plot(high_cardinality = 1))
  expect_doppelganger("Inspect-cat-card-storms",   storms %>%   inspect_cat %>% show_plot(high_cardinality = 1))
})

test_that("inspect_cat plot suppress labels", {
  expect_doppelganger("Inspect-cat-no-lab-starwars", starwars %>% inspect_cat %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-no-lab-tech", tech %>% inspect_cat %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-no-lab-storms",   storms %>%   inspect_cat %>% show_plot(text_labels = FALSE))
})

test_that("inspect_cat plot paired test", {
  expect_doppelganger("Inspect-cat-paired-starwars", starwars %>% inspect_cat(x2) %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-paired-storms",   storms %>%   inspect_cat(y2) %>% show_plot(text_labels = FALSE))
})

test_that("inspect_cat plot color palette", {
  expect_doppelganger("Inspect-cat-palette-starwars", starwars %>% inspect_cat() %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-cat-palette-storms",   storms %>%   inspect_cat() %>% show_plot(col_palette = 2))
})

test_that("inspect_cat plot label_thresh ", {
  expect_doppelganger("Inspect-cat-thresh-starwars_01", starwars %>% inspect_cat %>% show_plot(label_thresh = 0.01))
  expect_doppelganger("Inspect-cat-thresh-starwars_1", starwars %>% inspect_cat %>% show_plot(label_thresh = 0.1))
  expect_doppelganger("Inspect-cat-thresh-starwars_5", starwars %>% inspect_cat %>% show_plot(label_thresh = 0.5))
})
