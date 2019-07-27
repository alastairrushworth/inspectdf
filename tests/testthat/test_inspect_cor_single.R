library(vdiffr)
context("inspect_cor with single dataframe")

# load in some example data
data("starwars", package = "dplyr")
data("nasa", package = "dplyr")
data("band_instruments", package = "dplyr")
data("storms", package = "dplyr")
data(mtcars, airquality)

test_that("Output is a data frame", {
  expect_is(inspect_cor(mtcars), "data.frame")
  expect_is(inspect_cor(band_instruments), "data.frame")
  expect_error(inspect_cor(nasa))
  expect_is(inspect_cor(starwars), "data.frame")
  expect_is(inspect_cor(storms), "data.frame")
  expect_is(inspect_cor(airquality), "data.frame")
})


diff_correlatations <- function(data_input){
  x1 <- data_input %>% inspect_cor %>% dplyr::select(corr)
  x <- data_input %>% dplyr::select_if(is.numeric)
  x <- cor(x, use = "pairwise.complete.obs")
  diag(x) <- NA
  x2 <- suppressWarnings(x %>% c %>% tibble::as_tibble() %>% 
    dplyr::rename(corr = value) %>% dplyr::filter(!is.na(corr)) %>%
    dplyr::distinct() %>% dplyr::arrange(dplyr::desc(abs(corr))))
  return(mean(unlist(abs(x1 - x2))))
}

test_that("Output correlations are correct", {
  expect_lt(diff_correlatations(mtcars), 10^-15)
  expect_lt(diff_correlatations(starwars), 10^-15)
  expect_lt(diff_correlatations(storms), 10^-15)
  expect_lt(diff_correlatations(airquality), 10^-15)
})

test_that("inspect_cor & single column df = empty df", {
  expect_equal(nrow(inspect_cor(mtcars %>% select(1))), 0)
})

test_that("inspect_cor & constant column = NA", {
  expect_warning(inspect_cor(data.frame(z = 1:10, y = rep(1, 10))))
  expect_equal(suppressWarnings(sum(is.na(inspect_cor(data.frame(z = 1:10, y = rep(1, 10)))$corr))), 1)
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

test_that("Single df correlation plots work", {
  expect_doppelganger("Inspect-cor-starwars", starwars %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-storms",   storms %>% inspect_cor %>% show_plot)
  expect_doppelganger("Inspect-cor-storms_alpha", storms %>% inspect_cor %>% show_plot(alpha = 0.1))
})


test_that("Spaces in column names have no effect", {
  colnames(starwars)[1] <- c("name with spaces")
  x <- starwars %>% inspect_cor
  expect_is(x,  "data.frame")
  expect_doppelganger("inspect-cor-colnames-with-spaces", x %>% show_plot)
})

