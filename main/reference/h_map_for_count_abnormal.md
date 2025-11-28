# Helper function to create a map data frame for `trim_levels_to_map()`

**\[stable\]**

Helper function to create a map data frame from the input dataset, which
can be used as an argument in the `trim_levels_to_map` split function.
Based on different method, the map is constructed differently.

## Usage

``` r
h_map_for_count_abnormal(
  df,
  variables = list(anl = "ANRIND", split_rows = c("PARAM"), range_low = "ANRLO",
    range_high = "ANRHI"),
  abnormal = list(low = c("LOW", "LOW LOW"), high = c("HIGH", "HIGH HIGH")),
  method = c("default", "range"),
  na_str = "<Missing>"
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- abnormal:

  (named `list`)  
  identifying the abnormal range level(s) in `df`. Based on the levels
  of abnormality of the input dataset, it can be something like
  `list(Low = "LOW LOW", High = "HIGH HIGH")` or
  `abnormal = list(Low = "LOW", High = "HIGH"))`

- method:

  (`string`)  
  indicates how the returned map will be constructed. Can be `"default"`
  or `"range"`.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

## Value

A map `data.frame`.

## Note

If method is `"default"`, the returned map will only have the abnormal
directions that are observed in the `df`, and records with all normal
values will be excluded to avoid error in creating layout. If method is
`"range"`, the returned map will be based on the rule that at least one
observation with low range \> 0 for low direction and at least one
observation with high range is not missing for high direction.

## Examples

``` r
adlb <- df_explicit_na(tern_ex_adlb)

h_map_for_count_abnormal(
  df = adlb,
  variables = list(anl = "ANRIND", split_rows = c("LBCAT", "PARAM")),
  abnormal = list(low = c("LOW"), high = c("HIGH")),
  method = "default",
  na_str = "<Missing>"
)
#>        LBCAT                                PARAM ANRIND
#> 1  CHEMISTRY Alanine Aminotransferase Measurement    LOW
#> 4  CHEMISTRY Alanine Aminotransferase Measurement   HIGH
#> 7  CHEMISTRY Alanine Aminotransferase Measurement NORMAL
#> 2  CHEMISTRY       C-Reactive Protein Measurement    LOW
#> 3  CHEMISTRY       C-Reactive Protein Measurement   HIGH
#> 8  CHEMISTRY       C-Reactive Protein Measurement NORMAL
#> 5 IMMUNOLOGY         Immunoglobulin A Measurement    LOW
#> 6 IMMUNOLOGY         Immunoglobulin A Measurement   HIGH
#> 9 IMMUNOLOGY         Immunoglobulin A Measurement NORMAL

df <- data.frame(
  USUBJID = c(rep("1", 4), rep("2", 4), rep("3", 4)),
  AVISIT = c(
    rep("WEEK 1", 2),
    rep("WEEK 2", 2),
    rep("WEEK 1", 2),
    rep("WEEK 2", 2),
    rep("WEEK 1", 2),
    rep("WEEK 2", 2)
  ),
  PARAM = rep(c("ALT", "CPR"), 6),
  ANRIND = c(
    "NORMAL", "NORMAL", "LOW",
    "HIGH", "LOW", "LOW", "HIGH", "HIGH", rep("NORMAL", 4)
  ),
  ANRLO = rep(5, 12),
  ANRHI = rep(20, 12)
)
df$ANRIND <- factor(df$ANRIND, levels = c("LOW", "HIGH", "NORMAL"))
h_map_for_count_abnormal(
  df = df,
  variables = list(
    anl = "ANRIND",
    split_rows = c("PARAM"),
    range_low = "ANRLO",
    range_high = "ANRHI"
  ),
  abnormal = list(low = c("LOW"), high = c("HIGH")),
  method = "range",
  na_str = "<Missing>"
)
#>   PARAM ANRIND
#> 1   ALT    LOW
#> 3   ALT   HIGH
#> 5   ALT NORMAL
#> 2   CPR    LOW
#> 4   CPR   HIGH
#> 6   CPR NORMAL
```
