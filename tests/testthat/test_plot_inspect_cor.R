context("show_plot inspect_cor")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)
set.seed(21)
x2 <- starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
y2 <- storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)

# test_that("inspect_cor plots", {
#   expect_doppelganger("Inspect-cor-starwars", starwars %>% inspect_cor %>% show_plot)
#   expect_doppelganger("Inspect-cor-storms",   storms %>%   inspect_cor %>% show_plot)
#   expect_doppelganger("Inspect-cor-tech",   tech %>%   inspect_cor %>% show_plot)
# })

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

test_that("Grouped df correlation plots work", {
  expect_doppelganger("Inspect-cor-grouped-plot-starwars", starwars %>% dplyr::group_by(gender) %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-plot-tech-year", tech %>% dplyr::group_by(year) %>% inspect_cor() %>% show_plot)
  expect_doppelganger("Inspect-cor-grouped-plot-tech-quarter", tech %>% dplyr::group_by(quarter) %>% inspect_cor() %>% show_plot)
})