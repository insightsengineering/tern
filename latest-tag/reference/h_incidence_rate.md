# Helper functions for incidence rate

**\[stable\]**

## Usage

``` r
h_incidence_rate(person_years, n_events, control = control_incidence_rate())

h_incidence_rate_normal(person_years, n_events, alpha = 0.05)

h_incidence_rate_normal_log(person_years, n_events, alpha = 0.05)

h_incidence_rate_exact(person_years, n_events, alpha = 0.05)

h_incidence_rate_byar(person_years, n_events, alpha = 0.05)
```

## Arguments

- person_years:

  (`numeric(1)`)  
  total person-years at risk.

- n_events:

  (`integer(1)`)  
  number of events observed.

- control:

  (`list`)  
  parameters for estimation details, specified by using the helper
  function
  [`control_incidence_rate()`](https://insightsengineering.github.io/tern/reference/control_incidence_rate.md).
  Possible parameter options are:

  - `conf_level`: (`proportion`)  
    confidence level for the estimated incidence rate.

  - `conf_type`: (`string`)  
    `normal` (default), `normal_log`, `exact`, or `byar` for confidence
    interval type.

  - `input_time_unit`: (`string`)  
    `day`, `week`, `month`, or `year` (default) indicating time unit for
    data input.

  - `num_pt_year`: (`numeric`)  
    time unit for desired output (in person-years).

- alpha:

  (`numeric(1)`)  
  two-sided alpha-level for confidence interval.

## Value

Estimated incidence rate, `rate`, and associated confidence interval,
`rate_ci`.

## Functions

- `h_incidence_rate()`: Helper function to estimate the incidence rate
  and associated confidence interval.

- `h_incidence_rate_normal()`: Helper function to estimate the incidence
  rate and associated confidence interval based on the normal
  approximation for the incidence rate. Unit is one person-year.

- `h_incidence_rate_normal_log()`: Helper function to estimate the
  incidence rate and associated confidence interval based on the normal
  approximation for the logarithm of the incidence rate. Unit is one
  person-year.

- `h_incidence_rate_exact()`: Helper function to estimate the incidence
  rate and associated exact confidence interval. Unit is one
  person-year.

- `h_incidence_rate_byar()`: Helper function to estimate the incidence
  rate and associated Byar's confidence interval. Unit is one
  person-year.

## See also

[incidence_rate](https://insightsengineering.github.io/tern/reference/incidence_rate.md)

## Examples

``` r
h_incidence_rate_normal(200, 2)
#> $rate
#> [1] 0.01
#> 
#> $rate_ci
#> [1] -0.003859038  0.023859038
#> 

h_incidence_rate_normal_log(200, 2)
#> $rate
#> [1] 0.01
#> 
#> $rate_ci
#> [1] 0.002500977 0.039984382
#> 

h_incidence_rate_exact(200, 2)
#> $rate
#> [1] 0.01
#> 
#> $rate_ci
#> [1] 0.001211046 0.036123438
#> 

h_incidence_rate_byar(200, 2)
#> $rate
#> [1] 0.01
#> 
#> $rate_ci
#> [1] 0.001994207 0.032054171
#> 
```
