context("show_plot inspect_mem")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
z2 <- tech %>% dplyr::sample_n(1000, replace = T)
library(vdiffr)

test_that("inspect_mem plots", {
  expect_doppelganger("Inspect-mem-starwars", starwars %>% inspect_mem %>% show_plot)
  expect_doppelganger("Inspect-mem-storms",   storms %>%   inspect_mem %>% show_plot)
  expect_doppelganger("Inspect-mem-tech",   tech %>%   inspect_mem %>% show_plot)
})

test_that("inspect_mem plot paired", {
  expect_doppelganger("Inspect-mem-paired-starwars", starwars %>% inspect_mem(x2) %>% show_plot)
  expect_doppelganger("Inspect-mem-paired-storms",   storms %>%   inspect_mem(y2) %>% show_plot)
  expect_doppelganger("Inspect-mem-paired-tech",   tech %>%   inspect_mem(z2) %>% show_plot)
})

test_that("inspect_mem plot suppress labels", {
  expect_doppelganger("Inspect-mem-no-lab-starwars", starwars %>% inspect_mem %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-mem-no-lab-tech", tech %>% inspect_mem %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-mem-no-lab-storms",   storms %>%   inspect_mem %>% show_plot(text_labels = FALSE))
})

test_that("inspect_mem plot color palette", {
  expect_doppelganger("Inspect-mem-palette-starwars", starwars %>% inspect_mem() %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-mem-palette-storms",   storms %>%   inspect_mem() %>% show_plot(col_palette = 2))
})