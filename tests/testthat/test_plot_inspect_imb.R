context("plot inspect_imb plots")

# load in some example data
data("storms", package = "dplyr")
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
library(vdiffr)

test_that("inspect_imb plot basic", {
  expect_doppelganger("Inspect-imb-tdf", tdf %>% inspect_imb %>% plot)
  expect_doppelganger("Inspect-imb-storms",   storms %>%   inspect_imb %>% plot)
})

test_that("inspect_imb plot col_palette", {
  expect_doppelganger("Inspect-imb-palette-tdf", tdf %>% inspect_imb %>% plot(col_palette = 1))
  expect_doppelganger("Inspect-imb-palette-storms",   storms %>%   inspect_imb %>% plot(col_palette = 2))
})

test_that("inspect_imb plot paired", {
  expect_doppelganger("Inspect-imb-paired-tdf", tdf %>% inspect_imb(x2) %>% plot)
  expect_doppelganger("Inspect-imb-paired-storms",   storms %>%   inspect_imb(y2) %>% plot)
})

test_that("inspect_imb suppress labels", {
  expect_doppelganger("Inspect-imb-no-lab-tdf", tdf %>% inspect_imb %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-imb-no-lab-storms",   storms %>%   inspect_imb %>% plot(text_labels = FALSE))
})