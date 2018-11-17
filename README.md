

# reporter

Tools to explore large and messy data frames

__installation__ 

```r
devtools::install_github("alastairrushworth/reporter")
```


__usage__

```{r}
library(reporter)
# a data frame
data(mtcars)

# show what's in the data
mtcars %>% report
```