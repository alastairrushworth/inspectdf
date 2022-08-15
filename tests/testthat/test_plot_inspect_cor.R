context("plot inspect_cor")

# load in some example data
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_cor plots", {
  expect_doppelganger("Inspect-cor-tdf", tdf %>% inspect_cor %>% plot)
  expect_doppelganger("Inspect-cor-storms",   storms %>%   inspect_cor %>% plot)
})

test_that("inspect_cor spearman & kendall", {
  expect_doppelganger("Inspect-cor-kendall-tdf", tdf %>% inspect_cor(method = 'kendall') %>% plot)
  expect_doppelganger("Inspect-cor-spearman-storms",   storms %>%   inspect_cor(method = 'spearm') %>% plot)
})

test_that("inspect_cor plot paired", {
  expect_doppelganger("Inspect-cor-paired-tdf", tdf %>% inspect_cor(x2) %>% plot)
  expect_doppelganger("Inspect-cor-paired-storms",   storms %>%   inspect_cor(y2) %>% plot)
})

test_that("inspect_cor plot suppress labels", {
  expect_doppelganger("Inspect-cor-no-lab-tdf", tdf %>% inspect_cor %>% plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cor-no-lab-storms",   storms %>%   inspect_cor %>% plot(text_labels = FALSE))
})

test_that("inspect_cor plot grouped", {
  expect_doppelganger("Inspect-cor-grouped-tdf", tdf %>% dplyr::group_by(nationality) %>% inspect_cor %>% plot)
})

test_that("inspect_cor filtered plots", {
  expect_doppelganger("Inspect-cor-filtered-storms-0.5", inspect_cor(storms) %>% dplyr::filter(abs(corr) > 0.5) %>% plot)
  expect_doppelganger("Inspect-cor-filtered-tdf-0.5",   inspect_cor(tdf) %>% dplyr::filter(abs(corr) > 0.1) %>% plot)
})



test_that("Grouped df correlation plots work", {
  expect_doppelganger("Inspect-cor-grouped-plot-tdf", tdf %>% dplyr::group_by(nationality) %>% inspect_cor %>% plot)
})

test_that("Spaces in column names have no effect", {
  colnames(tdf)[1] <- c("name with spaces")
  x <- tdf %>% inspect_cor
  expect_is(x,  "data.frame")
  expect_doppelganger("inspect-cor-colnames-with-spaces", x %>% plot)
})

test_that("Pair comparison correlation plots work", {
  set.seed(1)
  expect_doppelganger(
    "Inspect-cor-tdf2", 
    tdf %>% 
      inspect_cor(
        tdf %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
      ) %>% plot)
  expect_doppelganger(
    "Inspect-cor-storms2",   storms %>% 
      inspect_cor(
        storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
      ) %>% plot)
})
