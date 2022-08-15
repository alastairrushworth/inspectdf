context("helpers")

# load in some example data
# example data frame
zx <- data.frame(x = rnorm(100, sd = 0.00001), y = 1, z = 1:100)

x <- 1:50
x <- rep(x, each = 100)
y <- rnorm(length(x))
z <- 1 + (0.5 + x / 50) * y + rnorm(length(x))
grp_dat  <- tibble(x = as.factor(x), y = y, z = z)

# get groupwed correlation
grouped_example <- grp_dat %>% group_by(x) 


test_that("format_size", {
  expect_equal(format_size(2.1245), "2.1")
  expect_equal(format_size(0.0004), "4e-04")
})

test_that("sumna", {
  expect_equal(sapply(tdf, sumna), colSums(sapply(tdf, is.na)))
})

test_that("check_variance", {
  expect_warning(check_variance(zx))
})

