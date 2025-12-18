# Get selected statistics names

Helper function to be used for creating `afun`.

## Usage

``` r
afun_selected_stats(.stats, all_stats)
```

## Arguments

- .stats:

  (`vector` or `NULL`)  
  input to the layout creating function. Note that `NULL` means in this
  context that all default statistics should be used.

- all_stats:

  (`character`)  
  all statistics which can be selected here potentially.

## Value

A `character` vector with the selected statistics.
