# Control function for subgroup treatment effect pattern (STEP) calculations

**\[stable\]**

This is an auxiliary function for controlling arguments for STEP
calculations.

## Usage

``` r
control_step(
  biomarker = NULL,
  use_percentile = TRUE,
  bandwidth,
  degree = 0L,
  num_points = 39L
)
```

## Arguments

- biomarker:

  (`numeric` or `NULL`)  
  optional provision of the numeric biomarker variable, which could be
  used to infer `bandwidth`, see below.

- use_percentile:

  (`flag`)  
  if `TRUE`, the running windows are created according to quantiles
  rather than actual values, i.e. the bandwidth refers to the percentage
  of data covered in each window. Suggest `TRUE` if the biomarker
  variable is not uniformly distributed.

- bandwidth:

  (`numeric(1)` or `NULL`)  
  indicating the bandwidth of each window. Depending on the argument
  `use_percentile`, it can be either the length of actual-value windows
  on the real biomarker scale, or percentage windows. If
  `use_percentile = TRUE`, it should be a number between 0 and 1. If
  `NULL`, treat the bandwidth to be infinity, which means only one
  global model will be fitted. By default, `0.25` is used for percentage
  windows and one quarter of the range of the `biomarker` variable for
  actual-value windows.

- degree:

  (`integer(1)`)  
  the degree of polynomial function of the biomarker as an interaction
  term with the treatment arm fitted at each window. If 0 (default),
  then the biomarker variable is not included in the model fitted in
  each biomarker window.

- num_points:

  (`integer(1)`)  
  the number of points at which the hazard ratios are estimated. The
  smallest number is 2.

## Value

A list of components with the same names as the arguments, except
`biomarker` which is just used to calculate the `bandwidth` in case that
actual biomarker windows are requested.

## Examples

``` r
# Provide biomarker values and request actual values to be used,
# so that bandwidth is chosen from range.
control_step(biomarker = 1:10, use_percentile = FALSE)
#> $use_percentile
#> [1] FALSE
#> 
#> $bandwidth
#> [1] 2.25
#> 
#> $degree
#> [1] 0
#> 
#> $num_points
#> [1] 39
#> 

# Use a global model with quadratic biomarker interaction term.
control_step(bandwidth = NULL, degree = 2)
#> $use_percentile
#> [1] TRUE
#> 
#> $bandwidth
#> NULL
#> 
#> $degree
#> [1] 2
#> 
#> $num_points
#> [1] 39
#> 

# Reduce number of points to be used.
control_step(num_points = 10)
#> $use_percentile
#> [1] TRUE
#> 
#> $bandwidth
#> [1] 0.25
#> 
#> $degree
#> [1] 0
#> 
#> $num_points
#> [1] 10
#> 
```
