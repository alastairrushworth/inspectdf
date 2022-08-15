context("plot inspect_num")

# load in some example data
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_num plots", {
  expect_doppelganger("Inspect-num-tdf", tdf %>% inspect_num %>% plot)
  expect_doppelganger("Inspect-num-storms",   storms %>%   inspect_num %>% plot)
})

test_that("inspect_num plot paired", {
  expect_doppelganger("Inspect-num-paired-tdf", tdf %>% inspect_num(x2) %>% plot)
  expect_doppelganger("Inspect-num-paired-storms",   storms %>%   inspect_num(y2) %>% plot)
})

test_that("inspect_num plot suppress labels", {
  expect_doppelganger("Inspect-num-no-lab-tdf", tdf %>% inspect_num %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-num-no-lab-storms",   storms %>%   inspect_num %>% plot(text_labels = FALSE))
})

test_that("inspect_num plot color palette", {
  expect_doppelganger("Inspect-num-palette-tdf", tdf %>% inspect_num() %>% plot(col_palette = 1))
  expect_doppelganger("Inspect-num-palette-storms",   storms %>%   inspect_num() %>% plot(col_palette = 2))
})

