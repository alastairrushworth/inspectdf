[![Build Status](https://travis-ci.org/alastairrushworth/reporter.svg?branch=master)](https://travis-ci.org/alastairrushworth/reporter)

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
data(starwars)
```


__Usage: Summarise size__

```{r}
starwars %>% report_space
```

```{r}
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

__Usage: Summarise column types__

```{r}
# show what's in the data
starwars %>% report_types(type = "console")

```
```
---------------------------------
* 13 columns composed of types: *
---------------------------------

               % of columns             (%) (#) Type 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■■·············· •  54% (7) character
    • ················■■■■■■■······· •  23% (3) list
    • ·······················■■■■■·· •  15% (2) numeric
    • ····························■■ •   8% (1) integer
```


__Usage: Summarise missing values__

```{r}
starwars %>% report_na(type = "console")
```

```{r}
-------------------------------
* Columns sorted by % missing *
-------------------------------

             % column missing          (%) Column 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■··············· • 51% birth_year
    • ■■■■■■■■■■···················· • 32% mass
    • ■■■··························· • 11% homeworld
    • ■■···························· •  7% height
    • ■■···························· •  6% hair_color
    • ■■···························· •  6% species
    • ■····························· •  3% gender
```

__Usage: Summarise correlations__

```{r}
starwars %>% report_cor(type = "console")
```

```{r}
---------------------------------
* Most correlated numeric pairs *
---------------------------------

        Absolute coefficient (|ρ|)     (ρ)    Column pair 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■················ • +0.478 mass & birth_year
    • ■■■■■■■■■■■■·················· • -0.400 height & birth_year
    • ■■■■·························· • +0.134 height & mass

```


__Usage: Summarise columns with imbalanced features__

```{r}
starwars %>% report_imbalance(type = "console")
```

```{r}
-----------------------------------------------
* Top most imbalanced features (exlc. numeric *
-----------------------------------------------

         % Single dominant value       (%) Column     Value  
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■■■■■■■········· • 71% gender     male
    • ■■■■■■■■■■■■■················· • 43% hair_color none
    • ■■■■■■■■■■■■·················· • 40% species    Human
    • ■■■■■■■······················· • 24% eye_color  brown
    • ■■■■■■························ • 20% skin_color fair
    • ■■■■·························· • 13% homeworld  Naboo
    • ······························ •  1% name       Ackbar
```


__Usage: Summarise association between categorical columns__

```{r}
starwars %>% report_association(type = "console")
```

```{r}
-------------------------------------
* Most associated categorical pairs *
-------------------------------------

        Goodman and Kruskal's tau      (τ)    Column pair 
    -------------------------------------------------------- 
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> hair_color
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> skin_color
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> eye_color
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> gender
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> homeworld
    • ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ • 1.000 name -> species
    • ■■■■■■■■■■■■■■■■■■■■■■········ • 0.722 homeworld -> species
    • ■■■■■■■■■■■■■■■■■■■··········· • 0.649 homeworld -> eye_color
    • ■■■■■■■■■■■■■■■■■■············ • 0.616 skin_color -> species
    • ■■■■■■■■■■■■■■■■■■············ • 0.612 homeworld -> skin_color
    • ■■■■■■■■■■■■■■■■■············· • 0.579 species -> eye_color
    • ■■■■■■■■■■■■■■■■■············· • 0.565 homeworld -> hair_color
    • ■■■■■■■■■■■■■■■■■············· • 0.558 homeworld -> name
    • ■■■■■■■■■■■■■■■■·············· • 0.543 species -> skin_color
    • ■■■■■■■■■■■■■■■■·············· • 0.534 species -> homeworld
    • ■■■■■■■■■■■■■■■■·············· • 0.524 skin_color -> gender
    • ■■■■■■■■■■■■■■■··············· • 0.514 skin_color -> eye_color
    • ■■■■■■■■■■■■■■■··············· • 0.510 skin_color -> hair_color
    • ■■■■■■■■■■■■■■················ • 0.480 species -> hair_color
    • ■■■■■■■■■■■■■■················ • 0.476 species -> gender
    • ■■■■■■■■■■■■■················· • 0.430 species -> name
    • ■■■■■■■■■■■■■················· • 0.417 homeworld -> gender
    • ■■■■■■■■■■■··················· • 0.371 skin_color -> homeworld
    • ■■■■■■■■■■···················· • 0.349 skin_color -> name
    • ■■■■■■■■■■···················· • 0.323 eye_color -> hair_color
    • ■■■■■■■■■■···················· • 0.318 eye_color -> species
    • ■■■■■■■■······················ • 0.260 hair_color -> eye_color
    • ■■■■■■■······················· • 0.235 eye_color -> skin_color
    • ■■■■■■■······················· • 0.232 hair_color -> species
    • ■■■■■■■······················· • 0.218 hair_color -> gender
    • ■■■■■■························ • 0.212 hair_color -> skin_color
    • ■■■■■························· • 0.182 eye_color -> gender
    • ■■■■■························· • 0.178 eye_color -> homeworld
    • ■■■■■························· • 0.163 eye_color -> name
    • ■■■■·························· • 0.147 hair_color -> homeworld
    • ■■■■·························· • 0.140 hair_color -> name
    • ■■■··························· • 0.106 gender -> species
    • ■■■··························· • 0.097 gender -> hair_color
    • ■■···························· • 0.062 gender -> skin_color
    • ■■···························· • 0.060 gender -> eye_color
    • ■■···························· • 0.056 gender -> homeworld
    • ■····························· • 0.047 gender -> name
```


