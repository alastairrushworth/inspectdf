context("show_plot inspect_cor")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

test_that("inspect_cor plots", {
  expect_doppelganger("Inspect-cor-starwars", starwars %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-storms",   storms %>%   inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-tech",   tech %>%   inspect_cor %>% show_plot)
})

test_that("inspect_cor spearman & kendall", {
  expect_doppelganger("Inspect-cor-kendall-starwars", starwars %>% inspect_cor(method = 'kendall') %>% show_plot)
  expect_doppelganger("Inspect-cor-spearman-storms",   storms %>%   inspect_cor(method = 'spearm') %>% show_plot)
  expect_doppelganger("Inspect-cor-kendall-tech",   tech %>%   inspect_cor(method = 'ken') %>% show_plot)
})

test_that("inspect_cor plot paired", {
  expect_doppelganger("Inspect-cor-paired-starwars", starwars %>% inspect_cor(x2) %>% show_plot)
  expect_doppelganger("Inspect-cor-paired-storms",   storms %>%   inspect_cor(y2) %>% show_plot)
})

test_that("inspect_cor plot suppress labels", {
  expect_doppelganger("Inspect-cor-no-lab-starwars", starwars %>% inspect_cor %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cor-no-lab-tech", tech %>% inspect_cor %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cor-no-lab-storms",   storms %>%   inspect_cor %>% show_plot(text_labels = FALSE))
})

test_that("inspect_cor plot grouped", {
  expect_doppelganger("Inspect-cor-grouped-starwars", starwars %>% dplyr::group_by(gender) %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-tech",   tech %>% dplyr::group_by(year) %>%  inspect_cor %>% show_plot)
})

test_that("inspect_cor filtered plots", {
  expect_doppelganger("Inspect-cor-filtered-storms-0.5", inspect_cor(storms) %>% dplyr::filter(abs(corr) > 0.5) %>% show_plot)
  expect_doppelganger("Inspect-cor-filtered-starwars-0.5",   inspect_cor(starwars) %>% dplyr::filter(abs(corr) > 0.1) %>% show_plot)
})



test_that("Grouped df correlation plots work", {
  expect_doppelganger("Inspect-cor-grouped-plot-starwars", starwars %>% dplyr::group_by(gender) %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-plot-tech-year", tech %>% dplyr::group_by(year) %>% inspect_cor() %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-plot-tech-quarter", tech %>% dplyr::group_by(quarter) %>% inspect_cor() %>% show_plot)
})

test_that("Spaces in column names have no effect", {
  colnames(starwars)[1] <- c("name with spaces")
  x <- starwars %>% inspect_cor
  expect_is(x,  "data.frame")
  expect_doppelganger("inspect-cor-colnames-with-spaces", x %>% show_plot)
})
