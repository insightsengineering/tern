library(test.nest)

test_lintr()
test_strict(
  exclude_from_man = c(
    # survival package
    # simpleWarning: partial match of 'std' to 'std.err'
    "a_mmrm.Rd",
    "basic_node_info.Rd",
    "df_explicit_na.Rd",
    "displayable.Rd",
    "full_apply_at_depth.Rd",
    "g_forest.Rd",
    "g_km.Rd",
    "invisible_node.Rd",
    "node-class.Rd",
    "node_format_data.Rd",
    "rapply_tree.Rd",
    "rsort_tree.Rd",
    "s_coxph_pairwise.Rd",
    "sub-sub-node-method.Rd",
    "t_coxph.Rd",
    "t_el_disposition.Rd",
    "t_el_forest_rsp.Rd",
    "t_el_forest_tte.Rd",
    "t_forest_tte.Rd",
    "t_km.Rd",
    "t_tte.Rd",
    "tabulate_pairwise.Rd",
    "to_rtable.Rd"
    # simpleWarning in model.matrix.default(Terms, mf, contrasts = contrast.arg): partial argument match of 'contrasts'
    # to 'contrasts.arg'
  )
)
test_regexp()
test_spell()
# test_usage() # quite a lot of unused functions - has to disable for now # nolint
test_indent()
