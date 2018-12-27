[![Build Status](https://travis-ci.org/alastairrushworth/reporter.svg?branch=master)](https://travis-ci.org/alastairrushworth/reporter)

# reporter

Tools to explore and compare data frames.

`reporter::report` is a wrapper for the following operations
- `report_space` show memory usage
- `report_types` show column types
- `report_levels` show summary of levels withing categorical features
- `report_na` show columns ranked by highest missingness
- `report_cor` show column pairs ranked by highest absolute correlations
- `report_imbalance` show non-numeric columns ranked by highest imbalance
- `report_association` show non-numeric column pairs ranked by highest Goodman & Kruskal's tau


__Package installation__ 

```r
devtools::install_github("alastairrushworth/reporter")

# load package
library(reporter)

# a data frame
data(starwars)
```


__Summarise dataframe size__

```{r}
starwars %>% report_space

----------------------------------------------------
* Data has 13 cols and 87 rows, occupying 55.27 Kb *
* Top columns in order of memory                   *
----------------------------------------------------

           % of total data size        (%) Column 
    -------------------------------------------------------- 
    • ■■■■■························· • 17% films
    • ■■■■■························· • 16% starships
    • ■■■■■························· • 15% name
    • ■■■■·························· • 14% vehicles
    • ■■■··························· • 9%  homeworld
    • ■■···························· • 7%  species
    • ■■···························· • 7%  skin_color
    • ■····························· • 4%  eye_color
    • ■····························· • 4%  hair_color
    • ■····························· • 3%  gender
    • ■····························· • 2%  mass
    • ■····························· • 2%  birth_year
    • ······························ • 1%  height
```

__Summarise column types__

```{r}
# show what's in the data
starwars %>% report_types

# A tibble: 4 x 3
  col_type  count_type percent
  <chr>          <dbl>   <dbl>
1 character          7   53.8 
2 list               3   23.1 
3 numeric            2   15.4 
4 integer            1    7.69
```


__Summarise levels in categorical features__

```{r}
starwars %>% report_levels

# A tibble: 7 x 5
  col_name   n_levels dom_level dom_percent levels           
  <chr>         <int> <chr>           <dbl> <list>           
1 eye_color        15 brown           24.1  <tibble [15 × 2]>
2 gender            4 male            71.3  <tibble [4 × 2]> 
3 hair_color       12 none            42.5  <tibble [12 × 2]>
4 homeworld        48 Naboo           12.6  <tibble [48 × 2]>
5 name             87 Ackbar           1.15 <tibble [87 × 2]>
6 skin_color       31 fair            19.5  <tibble [31 × 2]>
7 species          37 Human           40.2  <tibble [37 × 2]>
```


__Summarise missing values__

```{r}
starwars %>% report_na

# A tibble: 13 x 3
   col_name   count percent
   <chr>      <int>   <dbl>
 1 birth_year    44   50.6 
 2 mass          28   32.2 
 3 homeworld     10   11.5 
 4 height         6    6.90
 5 hair_color     5    5.75
 6 species        5    5.75
 7 gender         3    3.45
 8 name           0    0   
 9 skin_color     0    0   
10 eye_color      0    0   
11 films          0    0   
12 vehicles       0    0   
13 starships      0    0    
```

__Summarise correlations__

```{r}
starwars %>% report_cor

# A tibble: 3 x 4
  col_1  col_2      pair                correlation
  <chr>  <chr>      <chr>                     <dbl>
1 mass   birth_year mass & birth_year         0.478
2 height birth_year height & birth_year      -0.400
3 height mass       height & mass             0.134
```


__Summarise columns with imbalanced features__

```{r}
starwars %>% report_imbalance

# A tibble: 7 x 3
  col_name   value  percent
  <chr>      <chr>    <dbl>
1 gender     male     71.3 
2 hair_color none     42.5 
3 species    Human    40.2 
4 eye_color  brown    24.1 
5 skin_color fair     19.5 
6 homeworld  Naboo    12.6 
7 name       Ackbar    1.15
```


__Summarise association between categorical columns__

```{r}
starwars %>% report_association(type = "console")

 A tibble: 42 x 4
   col_1      col_2      pair                    k_tau
   <chr>      <chr>      <chr>                   <dbl>
 1 name       hair_color name -> hair_color      1    
 2 name       skin_color name -> skin_color      1    
 3 name       eye_color  name -> eye_color       1    
 4 name       gender     name -> gender          1    
 5 name       homeworld  name -> homeworld       1    
 6 name       species    name -> species         1    
 7 homeworld  species    homeworld -> species    0.722
 8 homeworld  eye_color  homeworld -> eye_color  0.649
 9 skin_color species    skin_color -> species   0.616
10 homeworld  skin_color homeworld -> skin_color 0.612
# ... with 32 more rows
```


