# Cut numeric vector into empirical quantile bins

**\[stable\]**

This cuts a numeric vector into sample quantile bins.

## Usage

``` r
cut_quantile_bins(
  x,
  probs = c(0.25, 0.5, 0.75),
  labels = NULL,
  type = 7,
  ordered = TRUE
)
```

## Arguments

- x:

  (`numeric`)  
  the continuous variable values which should be cut into quantile bins.
  This may contain `NA` values, which are then not used for the quantile
  calculations, but included in the return vector.

- probs:

  (`numeric`)  
  the probabilities identifying the quantiles. This is a sorted vector
  of unique `proportion` values, i.e. between 0 and 1, where the
  boundaries 0 and 1 must not be included.

- labels:

  (`character`)  
  the unique labels for the quantile bins. When there are `n`
  probabilities in `probs`, then this must be `n + 1` long.

- type:

  (`integer(1)`)  
  type of quantiles to use, see
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) for
  details.

- ordered:

  (`flag`)  
  should the result be an ordered factor.

## Value

- `cut_quantile_bins`: A `factor` variable with appropriately-labeled
  bins as levels.

## Note

Intervals are closed on the right side. That is, the first bin is the
interval `[-Inf, q1]` where `q1` is the first quantile, the second bin
is then `(q1, q2]`, etc., and the last bin is `(qn, +Inf]` where `qn` is
the last quantile.

## Examples

``` r
# Default is to cut into quartile bins.
cut_quantile_bins(cars$speed)
#>  [1] [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]  
#>  [7] [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]  
#> [13] [0%,25%]   [0%,25%]   [0%,25%]   (25%,50%]  (25%,50%]  (25%,50%] 
#> [19] (25%,50%]  (25%,50%]  (25%,50%]  (25%,50%]  (25%,50%]  (25%,50%] 
#> [25] (25%,50%]  (25%,50%]  (50%,75%]  (50%,75%]  (50%,75%]  (50%,75%] 
#> [31] (50%,75%]  (50%,75%]  (50%,75%]  (50%,75%]  (50%,75%]  (50%,75%] 
#> [37] (50%,75%]  (50%,75%]  (75%,100%] (75%,100%] (75%,100%] (75%,100%]
#> [43] (75%,100%] (75%,100%] (75%,100%] (75%,100%] (75%,100%] (75%,100%]
#> [49] (75%,100%] (75%,100%]
#> Levels: [0%,25%] < (25%,50%] < (50%,75%] < (75%,100%]

# Use custom quantiles.
cut_quantile_bins(cars$speed, probs = c(0.1, 0.2, 0.6, 0.88))
#>  [1] [0%,10%]   [0%,10%]   [0%,10%]   [0%,10%]   [0%,10%]   (10%,20%] 
#>  [7] (10%,20%]  (10%,20%]  (10%,20%]  (10%,20%]  (10%,20%]  (20%,60%] 
#> [13] (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%] 
#> [19] (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%] 
#> [25] (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%]  (20%,60%] 
#> [31] (20%,60%]  (60%,88%]  (60%,88%]  (60%,88%]  (60%,88%]  (60%,88%] 
#> [37] (60%,88%]  (60%,88%]  (60%,88%]  (60%,88%]  (60%,88%]  (60%,88%] 
#> [43] (60%,88%]  (60%,88%]  (88%,100%] (88%,100%] (88%,100%] (88%,100%]
#> [49] (88%,100%] (88%,100%]
#> Levels: [0%,10%] < (10%,20%] < (20%,60%] < (60%,88%] < (88%,100%]

# Use custom labels.
cut_quantile_bins(cars$speed, labels = paste0("Q", 1:4))
#>  [1] Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q1 Q2 Q2 Q2 Q2 Q2 Q2 Q2 Q2 Q2 Q2
#> [26] Q2 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q3 Q4 Q4 Q4 Q4 Q4 Q4 Q4 Q4 Q4 Q4 Q4 Q4
#> Levels: Q1 < Q2 < Q3 < Q4

# NAs are preserved in result factor.
ozone_binned <- cut_quantile_bins(airquality$Ozone)
which(is.na(ozone_binned))
#>  [1]   5  10  25  26  27  32  33  34  35  36  37  39  42  43  45  46  52  53  54
#> [20]  55  56  57  58  59  60  61  65  72  75  83  84 102 103 107 115 119 150
# So you might want to make these explicit.
explicit_na(ozone_binned)
#>   [1] (50%,75%]  (50%,75%]  [0%,25%]   [0%,25%]   <NA>       (25%,50%] 
#>   [7] (25%,50%]  (25%,50%]  [0%,25%]   <NA>       [0%,25%]   [0%,25%]  
#>  [13] [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   (50%,75%]  [0%,25%]  
#>  [19] (25%,50%]  [0%,25%]   [0%,25%]   [0%,25%]   [0%,25%]   (50%,75%] 
#>  [25] <NA>       <NA>       <NA>       (25%,50%]  (50%,75%]  (75%,100%]
#>  [31] (50%,75%]  <NA>       <NA>       <NA>       <NA>       <NA>      
#>  [37] <NA>       (25%,50%]  <NA>       (75%,100%] (50%,75%]  <NA>      
#>  [43] <NA>       (25%,50%]  <NA>       <NA>       (25%,50%]  (50%,75%] 
#>  [49] (25%,50%]  [0%,25%]   [0%,25%]   <NA>       <NA>       <NA>      
#>  [55] <NA>       <NA>       <NA>       <NA>       <NA>       <NA>      
#>  [61] <NA>       (75%,100%] (50%,75%]  (50%,75%]  <NA>       (75%,100%]
#>  [67] (50%,75%]  (75%,100%] (75%,100%] (75%,100%] (75%,100%] <NA>      
#>  [73] [0%,25%]   (25%,50%]  <NA>       [0%,25%]   (50%,75%]  (50%,75%] 
#>  [79] (50%,75%]  (75%,100%] (50%,75%]  [0%,25%]   <NA>       <NA>      
#>  [85] (75%,100%] (75%,100%] (25%,50%]  (50%,75%]  (75%,100%] (50%,75%] 
#>  [91] (75%,100%] (50%,75%]  (50%,75%]  [0%,25%]   [0%,25%]   (75%,100%]
#>  [97] (50%,75%]  (75%,100%] (75%,100%] (75%,100%] (75%,100%] <NA>      
#> [103] <NA>       (50%,75%]  (25%,50%]  (75%,100%] <NA>       (25%,50%] 
#> [109] (50%,75%]  (25%,50%]  (25%,50%]  (50%,75%]  (25%,50%]  [0%,25%]  
#> [115] <NA>       (50%,75%]  (75%,100%] (75%,100%] <NA>       (75%,100%]
#> [121] (75%,100%] (75%,100%] (75%,100%] (75%,100%] (75%,100%] (75%,100%]
#> [127] (75%,100%] (50%,75%]  (50%,75%]  (25%,50%]  (25%,50%]  (25%,50%] 
#> [133] (25%,50%]  (50%,75%]  (25%,50%]  (25%,50%]  [0%,25%]   [0%,25%]  
#> [139] (50%,75%]  [0%,25%]   [0%,25%]   (25%,50%]  [0%,25%]   [0%,25%]  
#> [145] (25%,50%]  (50%,75%]  [0%,25%]   [0%,25%]   (25%,50%]  <NA>      
#> [151] [0%,25%]   [0%,25%]   (25%,50%] 
#> Levels: [0%,25%] < (25%,50%] < (50%,75%] < (75%,100%] < <NA>
```
