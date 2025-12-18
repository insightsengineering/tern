# Custom split functions

**\[stable\]**

Collection of useful functions that are expanding on the core list of
functions provided by `rtables`. See
[rtables::custom_split_funs](https://insightsengineering.github.io/rtables/latest-tag/reference/custom_split_funs.html)
and
[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
for more information on how to make a custom split function. All these
functions work with
[`rtables::split_rows_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
argument `split_fun` to modify the way the split happens. For other
split functions, consider consulting
[`rtables::split_funcs`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_funcs.html).

## Usage

``` r
ref_group_position(position = "first")

level_order(order)
```

## Arguments

- position:

  (`string` or `integer`)  
  position to use for the reference group facet. Can be `"first"`,
  `"last"`, or a specific position.

- order:

  (`character` or `numeric`)  
  vector of ordering indices for the split facets.

## Value

- `ref_group_position()` returns an utility function that puts the
  reference group as first, last or at a certain position and needs to
  be assigned to `split_fun`.

&nbsp;

- `level_order()` returns an utility function that changes the original
  levels' order, depending on input `order` and split levels.

## Functions

- `ref_group_position()`: Split function to place reference group facet
  at a specific position during post-processing stage.

- `level_order()`: Split function to change level order based on an
  `integer` vector or a `character` vector that represent the split
  variable's factor levels.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)

## Examples

``` r
library(dplyr)

dat <- data.frame(
  x = factor(letters[1:5], levels = letters[5:1]),
  y = 1:5
)

# With rtables layout functions
basic_table() %>%
  split_cols_by("x", ref_group = "c", split_fun = ref_group_position("last")) %>%
  analyze("y") %>%
  build_table(dat)
#>         e      d      b      a      c  
#> ———————————————————————————————————————
#> Mean   5.00   4.00   2.00   1.00   3.00

# With tern layout funcitons
adtte_f <- tern_ex_adtte %>%
  filter(PARAMCD == "OS") %>%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0
  )

basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position("first")) %>%
  add_colcounts() %>%
  surv_time(
    vars = "AVAL",
    var_labels = "Survival Time (Months)",
    is_event = "is_event",
  ) %>%
  build_table(df = adtte_f)
#>                             ARM B          ARM A          ARM C    
#>                             (N=73)         (N=69)         (N=58)   
#> ———————————————————————————————————————————————————————————————————
#> Survival Time (Months)                                             
#>   Median                     23.9           32.0           20.8    
#>     95% CI               (18.3, 32.9)   (22.5, 49.3)   (12.9, 26.0)
#>   25% and 75%-ile         9.8, 42.0      17.4, 65.3     7.3, 37.1  
#>   Range                  0.1 to 154.1   0.3 to 155.5   0.6 to 80.7 

basic_table() %>%
  split_cols_by(var = "ARMCD", ref_group = "ARM B", split_fun = ref_group_position(2)) %>%
  add_colcounts() %>%
  surv_time(
    vars = "AVAL",
    var_labels = "Survival Time (Months)",
    is_event = "is_event",
  ) %>%
  build_table(df = adtte_f)
#>                             ARM A          ARM B          ARM C    
#>                             (N=69)         (N=73)         (N=58)   
#> ———————————————————————————————————————————————————————————————————
#> Survival Time (Months)                                             
#>   Median                     32.0           23.9           20.8    
#>     95% CI               (22.5, 49.3)   (18.3, 32.9)   (12.9, 26.0)
#>   25% and 75%-ile         17.4, 65.3     9.8, 42.0      7.3, 37.1  
#>   Range                  0.3 to 155.5   0.1 to 154.1   0.6 to 80.7 

# level_order --------
# Even if default would bring ref_group first, the original order puts it last
basic_table() %>%
  split_cols_by("Species", split_fun = level_order(c(1, 3, 2))) %>%
  analyze("Sepal.Length") %>%
  build_table(iris)
#>        setosa   virginica   versicolor
#> ——————————————————————————————————————
#> Mean    5.01      6.59         5.94   

# character vector
new_order <- level_order(levels(iris$Species)[c(1, 3, 2)])
basic_table() %>%
  split_cols_by("Species", ref_group = "virginica", split_fun = new_order) %>%
  analyze("Sepal.Length") %>%
  build_table(iris)
#>        setosa   virginica   versicolor
#> ——————————————————————————————————————
#> Mean    5.01      6.59         5.94   
```
