context("show_plot inspect_na")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_na plots", {
  expect_doppelganger("Inspect-na-starwars", starwars %>% inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-storms",   storms %>%   inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-tech",   tech %>%   inspect_na %>% show_plot)
})

test_that("inspect_na plot paired", {
  expect_doppelganger("Inspect-na-paired-starwars", starwars %>% inspect_na(x2) %>% show_plot)
  expect_doppelganger("Inspect-na-paired-storms",   storms %>%   inspect_na(y2) %>% show_plot)
})

test_that("inspect_na grouped plots", {
  expect_doppelganger("Inspect-na-grouped-starwars", starwars %>% group_by(gender) %>% inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-grouped-storms",   storms %>% group_by(month) %>%  inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-grouped-tech",   tech %>% group_by(year) %>% inspect_na %>% show_plot)
})

test_that("inspect_na plot suppress labels", {
  expect_doppelganger("Inspect-na-no-lab-starwars", starwars %>% inspect_na %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-na-no-lab-tech", tech %>% inspect_na %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-na-no-lab-storms", storms %>% inspect_na %>% show_plot(text_labels = FALSE))
})

test_that("inspect_na plot color palette", {
  expect_doppelganger("Inspect-na-palette-starwars", starwars %>% inspect_na() %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-na-palette-storms",   storms %>%   inspect_na() %>% show_plot(col_palette = 2))
})