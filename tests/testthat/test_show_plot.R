context("show_plot")

# load in some example data
data("starwars", package = "dplyr")
data("storms", package = "dplyr")
library(vdiffr)

test_that("inspect_cat plots", {
  expect_doppelganger("Inspect-cat-starwars", starwars %>% inspect_cat %>% show_plot)
  expect_doppelganger("Inspect-cat-starwars-card", starwars %>% inspect_cat %>% show_plot(high_cardinality = 1))
  expect_doppelganger("Inspect-cat-starwars-labels", starwars %>% inspect_cat %>% show_plot(text_labels = FALSE))
  expect_doppelganger("Inspect-cat-storms",   storms %>%   inspect_cat %>% show_plot)
  expect_doppelganger("Inspect-cat-storms-card",   storms %>%   inspect_cat %>% show_plot(high_cardinality = 30))
  expect_doppelganger("Inspect-cat-starwars2", 
                              starwars %>% 
                                inspect_cat(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                  ) %>% show_plot)
  expect_doppelganger("Inspect-cat-storms2",   storms %>% 
                                inspect_cat(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-cat-storms_col_1",   storms %>% inspect_cat %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-cat-storms_col_2",   storms %>% inspect_cat %>% show_plot(col_palette = 2))
})

test_that("inspect_cor plots", {
  expect_doppelganger("Inspect-cor-starwars", starwars %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-storms",   storms %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-starwars2", 
                              starwars %>% 
                                inspect_cor(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-cor-storms2",   storms %>% 
                                inspect_cor(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-cor-storms_alpha", storms %>% inspect_cor %>% show_plot(alpha = 0.1))
})

test_that("inspect_imb plots", {
  expect_doppelganger("Inspect-imb-starwars", starwars %>% inspect_imb %>% show_plot)
  expect_doppelganger("Inspect-imb-storms",   storms %>%   inspect_imb %>% show_plot)
  expect_doppelganger("Inspect-imb-starwars2", 
                              starwars %>% 
                                inspect_imb(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-imb-storms2",   storms %>% 
                                inspect_imb(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-imb-storms_col_1",   storms %>%   inspect_imb %>% show_plot(col_palette = 1))
  expect_doppelganger("Inspect-imb-storms_col_2",   storms %>%   inspect_imb %>% show_plot(col_palette = 2))
  
})

test_that("inspect_mem plots", {
  expect_doppelganger("Inspect-mem-starwars", starwars %>% inspect_mem %>% show_plot)
  expect_doppelganger("Inspect-mem-storms",   storms %>%   inspect_mem %>% show_plot)
  expect_doppelganger("Inspect-mem-starwars2", 
                              starwars %>% 
                                inspect_mem(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-mem-storms2",   storms %>% 
                                inspect_mem(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  
})

test_that("inspect_na plots", {
  expect_doppelganger("Inspect-na-starwars", starwars %>% inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-storms",   storms %>%   inspect_na %>% show_plot)
  expect_doppelganger("Inspect-na-starwars2", 
                              starwars %>% 
                                inspect_na(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-na-storms2",   storms %>% 
                                inspect_na(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  
})

test_that("inspect_num plots", {
  expect_doppelganger("Inspect-num-starwars", starwars %>% inspect_num %>% show_plot)
  expect_doppelganger("Inspect-num-storms",   storms %>%   inspect_num %>% show_plot)
  expect_doppelganger("Inspect-num-starwars2", 
                              starwars %>% 
                                inspect_num(
                                  starwars %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-num-storms2",   storms %>% 
                                inspect_num(
                                  storms %>% dplyr::select(-2) %>% dplyr::sample_n(100, replace = T)
                                ) %>% show_plot)
  expect_doppelganger("Inspect-num-storms_plot_layout",   storms %>%  inspect_num %>% show_plot(plot_layout = c(3, 4)))
})
