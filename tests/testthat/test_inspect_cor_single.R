context("inspect_cor with single dataframe")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output format checks", {
  z1 <- inspect_cor(starwars)
  z2 <- inspect_cor(tech)
  expect_is(z1, "data.frame")
  expect_is(z2, "data.frame")
  expect_equal(colnames(z1), c("col_1", "col_2", "corr", "p_value", "lower", "upper", "pcnt_nna"))
  expect_equal(colnames(z2), c("col_1", "col_2", "corr", "p_value", "lower", "upper", "pcnt_nna"))
})

diff_correlatations <- function(data_input, method){
  x1 <- data_input %>% inspect_cor(method = method) %>% dplyr::select(corr)
  x <- data_input %>% dplyr::select_if(is.numeric)
  x <- cor(x, use = "pairwise.complete.obs", method = method)
  diag(x) <- NA
  x2 <- suppressWarnings(x %>% c %>% tibble::as_tibble() %>% 
    dplyr::rename(corr = value) %>% dplyr::filter(!is.na(corr)) %>%
    dplyr::distinct() %>% dplyr::arrange(dplyr::desc(abs(corr))))
  return(mean(unlist(abs(x1 - x2))))
}

test_that("Correctness Kendall", {
  expect_lt(diff_correlatations(mtcars, method = "kendall"), 10^-15)
  expect_lt(diff_correlatations(mtcars, method = "kendall"), 10^-15)
  expect_lt(diff_correlatations(airquality, method = "kendall"), 10^-15)
})

test_that("Correctness Spearman", {
  expect_lt(diff_correlatations(mtcars, method = "spearman"), 10^-15)
  expect_lt(diff_correlatations(starwars, method = "spearman"), 10^-15)
  expect_lt(diff_correlatations(storms, method = "spearman"), 10^-15)
  expect_lt(diff_correlatations(airquality, method = "spearman"), 10^-15)
})

test_that("Correctness Pearson", {
  expect_lt(diff_correlatations(mtcars, method = "pearson"), 10^-15)
  expect_lt(diff_correlatations(starwars, method = "pearson"), 10^-15)
  expect_lt(diff_correlatations(storms, method = "pearson"), 10^-15)
  expect_lt(diff_correlatations(airquality, method = "pearson"), 10^-15)
})

test_that("inspect_cor & single column df = empty df", {
  expect_equal(nrow(inspect_cor(mtcars %>% select(1))), 0)
})

test_that("inspect_cor::with_col", {
  x <- inspect_cor(starwars, with_col = "mass")
  expect_is(x, "data.frame")
  expect_equal(nrow(x), 2)
  expect_equal(unique(x$col_1), "mass")
  x <- inspect_cor(iris, with_col = "Sepal.Length")
  expect_is(x, "data.frame")
  expect_equal(nrow(x), 3)
  expect_equal(unique(x$col_1), "Sepal.Length")
  # error - column not found
  expect_error(inspect_cor(iris, with_col = "mass"))
  # error - column not numeric
  expect_error(inspect_cor(starwars, with_col = "gender"))
})

test_that("filter & inspect_cor", {
  x <- inspect_cor(storms, with_col = "pressure")
  y <- x %>% 
    filter(col_2 %in% c("lat", "year", "wind"))
  expect_equal(attr(y, "df_names")$df1, "storms")
  expect_equal(attr(y, "method"), "pearson")
})


test_that("kendal and spearman work", {
  x <- inspect_cor(iris, method = "spearman")
  expect_is(x, "data.frame")
  y <- inspect_cor(iris, method = "kendall")
  expect_is(y, "data.frame")
})

test_that("kendal and spearman work", {
  z1 <- inspect_cor(starwars)
  nna_1 = mean((is.na(starwars$birth_year) + is.na(starwars$mass)) > 0) * 100
  nna_2 = mean((is.na(starwars$birth_year) + is.na(starwars$height)) > 0) * 100
  expect_equal(z1$pcnt_nna[1], 100 - nna_1)
  expect_equal(z1$pcnt_nna[2], 100 - nna_2)
})

