context("inspect_na single dataframe")

data("starwars", package = "dplyr")
starwars$mass_lg <- starwars$mass > 70

test_that("Proportion test is correct", {
  d1 <- starwars %>% select(birth_year)
  d2 <- starwars[1:20, ] %>% select(birth_year)
  p1 <- inspect_na(d1, d2) %>% select(6) %>% as.numeric
  p2 <- prop.test(c(sum(is.na(d1)), sum(is.na(d2))), c(nrow(d1), nrow(d2)))$p.value
  expect_equal(p1, p2)
})

