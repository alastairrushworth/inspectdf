context("plot inspect_mem")

# load in some example data
data("storms", package = "dplyr")
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
library(vdiffr)

test_that("inspect_mem plots", {
  expect_doppelganger("Inspect-mem-tdf", tdf %>% inspect_mem %>% plot)
  expect_doppelganger("Inspect-mem-storms",   storms %>%   inspect_mem %>% plot)
})

test_that("inspect_mem plot paired", {
  expect_doppelganger("Inspect-mem-paired-tdf", tdf %>% inspect_mem(x2) %>% plot)
  expect_doppelganger("Inspect-mem-paired-storms",   storms %>%   inspect_mem(y2) %>% plot)
})

test_that("inspect_mem plot suppress labels", {
  expect_doppelganger("Inspect-mem-no-lab-tdf", tdf %>% inspect_mem %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-mem-no-lab-storms",   storms %>%   inspect_mem %>% plot(text_labels = FALSE))
})

test_that("inspect_mem plot color palette", {
  expect_doppelganger("Inspect-mem-palette-tdf", tdf %>% inspect_mem() %>% plot(col_palette = 1))
  expect_doppelganger("Inspect-mem-palette-storms",   storms %>%   inspect_mem() %>% plot(col_palette = 2))
})