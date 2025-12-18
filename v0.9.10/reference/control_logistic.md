# Control function for logistic regression model fitting

**\[stable\]**

This is an auxiliary function for controlling arguments for logistic
regression models. `conf_level` refers to the confidence level used for
the Odds Ratio CIs.

## Usage

``` r
control_logistic(response_definition = "response", conf_level = 0.95)
```

## Arguments

- response_definition:

  (`string`)  
  the definition of what an event is in terms of `response`. This will
  be used when fitting the logistic regression model on the left hand
  side of the formula. Note that the evaluated expression should result
  in either a logical vector or a factor with 2 levels. By default this
  is just `"response"` such that the original response variable is used
  and not modified further.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A list of components with the same names as the arguments.

## Examples

``` r
# Standard options.
control_logistic()
#> $response_definition
#> [1] "response"
#> 
#> $conf_level
#> [1] 0.95
#> 

# Modify confidence level.
control_logistic(conf_level = 0.9)
#> $response_definition
#> [1] "response"
#> 
#> $conf_level
#> [1] 0.9
#> 

# Use a different response definition.
control_logistic(response_definition = "I(response %in% c('CR', 'PR'))")
#> $response_definition
#> [1] "I(response %in% c('CR', 'PR'))"
#> 
#> $conf_level
#> [1] 0.95
#> 
```
