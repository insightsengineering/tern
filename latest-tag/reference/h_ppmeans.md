# Function to return the estimated means using predicted probabilities

For each arm level, the predicted mean rate is calculated using the
fitted model object, with `newdata` set to the result of
[`stats::model.frame`](https://rdrr.io/r/stats/model.frame.html), a
reconstructed data or the original data, depending on the object formula
(coming from the fit). The confidence interval is derived using the
`conf_level` parameter.

## Usage

``` r
h_ppmeans(obj, .df_row, arm, conf_level)
```

## Arguments

- obj:

  (`glm.fit`)  
  fitted model object used to derive the mean rate estimates in each
  treatment arm.

- .df_row:

  (`data.frame`)  
  dataset that includes all the variables that are called in `.var` and
  `variables`.

- arm:

  (`string`)  
  group variable, for which the covariate adjusted means of multiple
  groups will be summarized. Specifically, the first level of `arm`
  variable is taken as the reference group.

- conf_level:

  (`proportion`)  
  value used to derive the confidence interval for the rate.

## Value

- `h_ppmeans()` returns the estimated means.

## See also

[`summarize_glm_count()`](https://insightsengineering.github.io/tern/reference/summarize_glm_count.md).
