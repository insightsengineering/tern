

# TODO for release v0.5.1

- table functions should use `col_N` arguments
    - use new `check_col_by` with `col_N` argument

- use `rbind.rtable` and `rbindl_rtables` with the `gap` argument instead of the
`*stack*` functions in tern

    - may check with `grep -R --include=*.R "check_col_by" ./`

- use new `rtabulate` version in combination with `header_add_N`

- use `# TODO:` as a tag to mark actions or clarifications.

- Resolve warning Missing object imported by a ':::' call: `rtables:::rtabulate_header`
for `t_tte` and `tabulate_pairwise`
    
- Document arguments:

  - `g_km` 'gp' 'vp' 'name'
  - `t_count_unique` 'na.rm' 'row.name' 'indent'
  - `with_label` 'x' 'label

- Documentation:
  - add `@seealso` section to every function documentation
  
