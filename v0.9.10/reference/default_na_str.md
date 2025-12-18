# Default string replacement for `NA` values

**\[stable\]**

The default string used to represent `NA` values. This value is used as
the default value for the `na_str` argument throughout the `tern`
package, and printed in place of `NA` values in output tables. If not
specified for each `tern` function by the user via the `na_str`
argument, or in the R environment options via `set_default_na_str()`,
then `NA` is used.

## Usage

``` r
default_na_str()

set_default_na_str(na_str)
```

## Arguments

- na_str:

  (`string`)  
  single string value to set in the R environment options as the default
  value to replace `NA`s. Use `getOption("tern_default_na_str")` to
  check the current value set in the R environment (defaults to `NULL`
  if not set).

## Value

- `default_na_str` returns the current value if an R environment option
  has been set for `"tern_default_na_str"`, or `NA_character_`
  otherwise.

&nbsp;

- `set_default_na_str` has no return value.

## Functions

- `default_na_str()`: Accessor for default `NA` value replacement
  string.

- `set_default_na_str()`: Setter for default `NA` value replacement
  string. Sets the option `"tern_default_na_str"` within the R
  environment.

## Examples

``` r
# Default settings
default_na_str()
#> [1] NA
getOption("tern_default_na_str")
#> NULL

# Set custom value
set_default_na_str("<Missing>")

# Settings after value has been set
default_na_str()
#> [1] "<Missing>"
getOption("tern_default_na_str")
#> [1] "<Missing>"
```
