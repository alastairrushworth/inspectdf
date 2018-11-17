

# reporter

Tools to explore large and messy data frames.

`reporter::report` is a wrapper for the following operations
- `report_space` show memory usage
- `report_types` show column types
- `report_na` show columns ranked by highest missingness
- `report_cor` show column pairs ranked by highest absolute correlations
- `report_imbalance` show non-numeric columns ranked by highest imbalance
- `report_association` show non-numeric column pairs ranked by highest Goodman & Kruskal's tau


__Installation__ 

```r
devtools::install_github("alastairrushworth/reporter")

# load package
library(reporter)

# a data frame
data(mtcars)
```


__Usage: space__

```{r}
mtcars %>% report_space
```

```{r}
---------------------------------------------------
* Data has 11 cols and 32 rows, occupying 7.04 Kb *
* Top columns in order of memory                  *
---------------------------------------------------

           % of total data size        (%) Column 
    -------------------------------------------------------- 
    • ■■■··························· • 9%  mpg
    • ■■■··························· • 9%  cyl
    • ■■■··························· • 9%  disp
    • ■■■··························· • 9%  hp
    • ■■■··························· • 9%  drat
    • ■■■··························· • 9%  wt
    • ■■■··························· • 9%  qsec
    • ■■■··························· • 9%  vs
    • ■■■··························· • 9%  am
    • ■■■··························· • 9%  gear
```

__Usage: types__

```{r}
# show what's in the data
mtcars %>% report_types

```
```
---------------------------------
* 11 columns composed of types: *
---------------------------------

               % of columns             (%)  (#)  Type 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ •  100% (11) numeric
```


__Usage: missing values__

```{r}
mtcars %>% report_na
```

```{r}
-------------------------------
* Columns sorted by % missing *
-------------------------------

    << Not applicable >>
```

__Usage: correlation__

```{r}
mtcars %>% report_cor
```

```{r}
---------------------------------
* Most correlated numeric pairs *
---------------------------------

        Absolute coefficient (|ρ|)     (ρ)    Column pair 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■··· • +0.902 cyl & disp
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■··· • +0.888 disp & wt
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■···· • -0.868 mpg & wt
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■···· • -0.852 mpg & cyl
    • ■■■■■■■■■■■■■■■■■■■■■■■■■····· • -0.848 mpg & disp
    • ■■■■■■■■■■■■■■■■■■■■■■■■■····· • +0.832 cyl & hp
    • ■■■■■■■■■■■■■■■■■■■■■■■■······ • -0.811 cyl & vs
    • ■■■■■■■■■■■■■■■■■■■■■■■■······ • +0.794 am & gear
    • ■■■■■■■■■■■■■■■■■■■■■■■■······ • +0.791 disp & hp
    • ■■■■■■■■■■■■■■■■■■■■■■■······· • +0.782 cyl & wt

```


__Usage: imbalance__

```{r}
mtcars %>% report_imbalance
```

```{r}
-----------------------------------------------
* Top most imbalanced features (exlc. numeric *
-----------------------------------------------

    << Not applicable >>
```


__Usage: association__

```{r}
mtcars %>% report_association
```

```{r}
-------------------------------------
* Most associated categorical pairs *
-------------------------------------

    << Not applicable >>
```


