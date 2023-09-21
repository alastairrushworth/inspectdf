context("show_plot inspect_num")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
shifted_x <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T) %>% mutate_if(is.numeric, ~.x -5)
expanded_y <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T) %>% mutate_if(is.numeric, ~.x * 1.75)

test_that("inspect_num plots", {
  expect_doppelganger("Inspect-num-starwars", starwars %>% inspect_num %>% show_plot)
  expect_doppelganger("Inspect-num-storms",   storms %>%   inspect_num %>% show_plot)
  expect_doppelganger("Inspect-num-tech",   tech %>%   inspect_num %>% show_plot)
})

test_that("inspect_num plot paired with different columns set", {
  expect_doppelganger("Inspect-num-paired-starwars", starwars %>% inspect_num(x2) %>% show_plot)
  expect_doppelganger("Inspect-num-paired-storms",   storms %>%   inspect_num(y2) %>% show_plot)
})

test_that("inspect_num plot paired with different value range", {
  expect_doppelganger("Inspect-num-paired-range-starwars", inspect_num(starwars, shifted_x) %>% show_plot)
  expect_doppelganger("Inspect-num-paired-range-storms",   inspect_num(storms, expanded_y)%>% show_plot)
})

test_that("inspect_num plot suppress labels", {
  expect_doppelganger("Inspect-num-no-lab-starwars", starwars %>% inspect_num %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-num-no-lab-tech", tech %>% inspect_num %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-num-no-lab-storms",   storms %>%   inspect_num %>% show_plot(text_labels = FALSE))
})

test_that("inspect_num plot color palette", {
  expect_doppelganger("Inspect-num-palette-starwars", starwars %>% inspect_num() %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-num-palette-storms",   storms %>%   inspect_num() %>% show_plot(col_palette = 2))
})

