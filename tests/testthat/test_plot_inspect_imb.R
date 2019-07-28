context("show_plot inspect_imb plots")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
library(vdiffr)

test_that("inspect_imb plot basic", {
  expect_doppelganger("Inspect-imb-starwars", starwars %>% inspect_imb %>% show_plot)
  expect_doppelganger("Inspect-imb-storms",   storms %>%   inspect_imb %>% show_plot)
  expect_doppelganger("Inspect-imb-tech",   tech %>%  inspect_imb %>% show_plot)  
})

test_that("inspect_imb plot col_palette", {
  expect_doppelganger("Inspect-imb-palette-starwars", starwars %>% inspect_imb %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-imb-palette-storms",   storms %>%   inspect_imb %>% show_plot(col_palette = 2))
})

test_that("inspect_imb plot paired", {
  expect_doppelganger("Inspect-imb-paired-starwars", starwars %>% inspect_imb(x2) %>% show_plot)
  expect_doppelganger("Inspect-imb-paired-storms",   storms %>%   inspect_imb(y2) %>% show_plot)
})

test_that("inspect_imb suppress labels", {
  expect_doppelganger("Inspect-imb-no-lab-starwars", starwars %>% inspect_imb %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-imb-no-lab-tech", tech %>% inspect_imb %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-imb-no-lab-storms",   storms %>%   inspect_imb %>% show_plot(text_labels = FALSE))
})