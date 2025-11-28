# Utilities to handle extra arguments in analysis functions

**\[stable\]** Important additional parameters, useful to modify
behavior of analysis and summary functions are listed in
[rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).
With these utility functions we can retrieve a curated list of these
parameters from the environment, and pass them to the analysis functions
with dedicated `...`; notice that the final `s_*` function will get them
through argument matching.

## Usage

``` r
retrieve_extra_afun_params(extra_afun_params)

get_additional_afun_params(add_alt_df = FALSE)
```

## Arguments

- extra_afun_params:

  (`list`)  
  list of additional parameters (`character`) to be retrieved from the
  environment. Curated list is present in
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html).

- add_alt_df:

  (`logical`)  
  if `TRUE`, the function will also add `.alt_df` and `.alt_df_row`
  parameters.

## Value

- `retrieve_extra_afun_params` returns a list of the values of the
  parameters in the environment.

&nbsp;

- `get_additional_afun_params` returns a list of additional parameters.

## Functions

- `retrieve_extra_afun_params()`: Retrieve additional parameters from
  the environment.

- `get_additional_afun_params()`: Curated list of additional parameters
  for analysis functions. Please check
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)
  for precise descriptions.
