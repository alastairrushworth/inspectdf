

# reporter

Tools to explore large and messy data frames

__Installation__ 

```r
devtools::install_github("alastairrushworth/reporter")
```


__Usage__

```{r}
library(reporter)

# a data frame
data(mtcars)

# show what's in the data
mtcars %>% report
```