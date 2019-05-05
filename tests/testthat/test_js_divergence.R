context("js_divergence_vec")

test_that("jensen-shannon is correctly calculated", {
  suppressWarnings(library(dplyr))
  set.seed(10)
  star_1 <- starwars %>% sample_n(50) %>% select(-birth_year)
  star_2 <- starwars %>% sample_n(50) %>% select(-1, -2)
  # find out why cat needs fisher to be approx?
  z <- inspect_num(star_1, star_2)
  p <- z$hist_1[[2]]$prop
  q <- z$hist_2[[2]]$prop
  log_zero <- function(g){
    ifelse(g == 0, NA, log(g)/log(2.0))
  }
  js_divergence <- function(p, q){
    m <- 0.5 * (p + q)
    0.5 * (sum(p * log_zero(p / m), na.rm = T) + sum(q * log_zero(q / m), na.rm = T))
  }
  js_stat <- js_divergence(p, q)
  js_stat2 <- z$jsd[2]
  expect_equal(js_stat, js_stat2)
})
