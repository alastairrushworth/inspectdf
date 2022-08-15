context("plot inspect_cat")

# load in some example data
data("storms", package = "dplyr")
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
library(vdiffr)

test_that("inspect_cat plot basic", {
  expect_doppelganger("Inspect-cat-tdf", tdf %>% inspect_cat %>% plot)
  expect_doppelganger("Inspect-cat-storms",   storms %>%   inspect_cat %>% plot)
})
  
test_that("inspect_cat plot cardinality", {
  expect_doppelganger("Inspect-cat-card-tdf", tdf %>% inspect_cat %>% plot(high_cardinality = 1))
  expect_doppelganger("Inspect-cat-card-storms",   storms %>%   inspect_cat %>% plot(high_cardinality = 1))
})

test_that("inspect_cat plot suppress labels", {
  expect_doppelganger("Inspect-cat-no-lab-tdf", tdf %>% inspect_cat %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-no-lab-storms",   storms %>%   inspect_cat %>% plot(text_labels = FALSE))
})

test_that("inspect_cat plot paired test", {
  expect_doppelganger("Inspect-cat-paired-tdf", tdf %>% inspect_cat(x2) %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-paired-storms",   storms %>%   inspect_cat(y2) %>% plot(text_labels = FALSE))
})

test_that("inspect_cat plot color palette", {
  expect_doppelganger("Inspect-cat-palette-tdf", tdf %>% inspect_cat() %>% plot(col_palette = 1))
  expect_doppelganger("Inspect-cat-palette-storms",   storms %>%   inspect_cat() %>% plot(col_palette = 2))
})

test_that("inspect_cat plot label_thresh ", {
  expect_doppelganger("Inspect-cat-thresh-tdf_01", tdf %>% inspect_cat %>% plot(label_thresh = 0.01))
  expect_doppelganger("Inspect-cat-thresh-tdf_1", tdf %>% inspect_cat %>% plot(label_thresh = 0.1))
  expect_doppelganger("Inspect-cat-thresh-tdf_5", tdf %>% inspect_cat %>% plot(label_thresh = 0.5))
})

test_that("Plot descending by imbalance", {
  expect_doppelganger(
    "Inspect-cat-single-descending-imbalance", 
    inspect_cat(tdf) %>% 
      dplyr::arrange(desc(common)) %>%
      plot())
})

test_that("Filter top 3 most significant difference pairs", {
  set.seed(10)
  expect_doppelganger(
    "Inspect-cat-pairs-filter-3", 
    inspect_cat(tdf, tdf %>% dplyr::sample_n(100, replace = T)) %>%
      dplyr::arrange(pval) %>%
      dplyr::slice(1:3) %>%
      plot())
})


