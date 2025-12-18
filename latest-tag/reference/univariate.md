# Univariate formula special term

**\[stable\]**

The special term `univariate` indicate that the model should be fitted
individually for every variable included in univariate.

## Usage

``` r
univariate(x)
```

## Arguments

- x:

  (`character`)  
  a vector of variable names separated by commas.

## Value

When used within a model formula, produces univariate models for each
variable provided.

## Details

If provided alongside with pairwise specification, the model
`y ~ ARM + univariate(SEX, AGE, RACE)` lead to the study and comparison
of the models

- `y ~ ARM`

- `y ~ ARM + SEX`

- `y ~ ARM + AGE`

- `y ~ ARM + RACE`
