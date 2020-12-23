context("js_divergence_vec")

test_that("jensen-shannon divergence is 1 when no distribution overlap", {
  suppressWarnings(library(dplyr))
  js_stat <- js_divergence(
    c(0, 0, 0, 1), 
    c(1, 0, 0, 0)
  )
  expect_equal(js_stat, 1)
})

test_that("jensen-shannon divergence is 0 when distributions identical", {
  suppressWarnings(library(dplyr))
  js_stat <- js_divergence(
    c(0, 0, 0, 1), 
    c(0, 0, 0, 1)
  )
  expect_equal(js_stat, 0)
})