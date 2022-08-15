context("inspect_na single dataframe")

tdf$height_lg <- tdf$height > 160

test_that("Proportion test is correct", {
  d1 <- tdf %>% select(distance)
  d2 <- tdf[1:20, ] %>% select(distance)
  p1 <- inspect_na(d1, d2) %>% select(6) %>% as.numeric
  p2 <- prop.test(c(sum(is.na(d1)), sum(is.na(d2))), c(nrow(d1), nrow(d2)))$p.value
  expect_equal(p1, p2)
})

