

# TODO for release 0.5.1

- table functions should use `col_N` arguments
    - use new `check_col_by` with `col_N` argument

- use `rbind.rtable` and `rbindl_rtables` with the `gap` argument instead of the
`*stack*` functions in tern

    - may check with `grep -R --include=*.R "check_col_by" ./`

- use new `rtabulate` version in combination with `header_add_N`



    