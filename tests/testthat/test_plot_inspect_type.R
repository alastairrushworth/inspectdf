context("plot inspect_types")

# load in some example data
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_types plots", {
  expect_doppelganger("Inspect-types-tdf", tdf %>% inspect_types %>% plot)
  expect_doppelganger("Inspect-types-storms",   storms %>%   inspect_types %>% plot)
})

test_that("inspect_types plot suppress labels", {
  expect_doppelganger("Inspect-types-no-lab-tdf", tdf %>% inspect_types %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-types-no-lab-storms",   storms %>%   inspect_types %>% plot(text_labels = FALSE))
})

test_that("inspect_types plot paired", {
  expect_doppelganger("Inspect-types-paired-tdf", tdf %>% inspect_types(x2) %>% plot)
  expect_doppelganger("Inspect-types-paired-storms",   storms %>%   inspect_types(y2) %>% plot)
})



