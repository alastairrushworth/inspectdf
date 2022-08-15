context("helpers")

# example data frame
zx <- data.frame(x = rnorm(100, sd = 0.00001), y = 1, z = 1:100)

x <- 1:50
x <- rep(x, each = 100)
y <- rnorm(length(x))
z <- 1 + (0.5 + x / 50) * y + rnorm(length(x))
grp_dat  <- tibble(x = as.factor(x), y = y, z = z)

# get grouped correlation
grouped_example <- grp_dat %>% group_by(x) 

test_that("check_dfs", {
  x <- list(a = c(1, 2), b = c("a", "b"))
  expect_error(check_df_cols(x))
  expect_silent(check_df_cols(mtcars))
  y <- data.frame()
  expect_error(check_df_cols(y))
})

test_that("check_dfs returns correct input type", {
  expect_equal(check_df_cols(df1 = grouped_example), "grouped")
  expect_equal(check_df_cols(df1 = tdf, df2 = tdf), "pair")
  expect_equal(check_df_cols(df1 = tdf, df2 = NULL), "single")
  expect_error(check_df_cols(df1 = grouped_example, 
                             df2 = grouped_example))
})

