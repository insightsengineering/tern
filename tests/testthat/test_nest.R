library(test.nest)

test_lintr()
test_strict(
  exclude_from_man = c(
    # survival package
    # simpleWarning: partial match of 'std' to 'std.err'
    "t_forest_tte.Rd",
    "t_el_forest_tte.Rd",
    "g_forest.Rd",
    "t_km.Rd",
    "g_km.Rd",
    "t_tte.Rd",
    # simpleWarning in model.matrix.default(Terms, mf, contrasts = contrast.arg): partial argument match of 'contrasts'
    # to 'contrasts.arg'
    "s_coxph_pairwise.Rd",
    "t_coxph.Rd"
  )
)
test_regexp()
test_spell()
# test_usage() # quite a lot of unused functions - has to disable for now # nolint
test_indent()
