# Update labels according to control specifications

**\[stable\]**

Given a list of statistic labels and and a list of control parameters,
updates labels with a relevant control specification. For example, if
control has element `conf_level` set to `0.9`, the default label for
statistic `mean_ci` will be updated to `"Mean 90% CI"`. Any labels that
are supplied via `labels_custom` will not be updated regardless of
`control`.

## Usage

``` r
labels_use_control(labels_default, control, labels_custom = NULL)
```

## Arguments

- labels_default:

  (named `character`)  
  a named vector of statistic labels to modify according to the control
  specifications. Labels that are explicitly defined in `labels_custom`
  will not be affected.

- control:

  (named `list`)  
  list of control parameters to apply to adjust default labels.

- labels_custom:

  (named `character`)  
  named vector of labels that are customized by the user and should not
  be affected by `control`.

## Value

A named character vector of labels with control specifications applied
to relevant labels.

## Examples

``` r
control <- list(conf_level = 0.80, quantiles = c(0.1, 0.83), test_mean = 0.57)
get_labels_from_stats(c("mean_ci", "quantiles", "mean_pval")) %>%
  labels_use_control(control = control)
#>                          mean_ci                        quantiles 
#>                    "Mean 80% CI"                "10% and 83%-ile" 
#>                        mean_pval 
#> "Mean p-value (H0: mean = 0.57)" 
```
