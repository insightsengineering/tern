library(test.nest)

#test_lintr() # commented-out because very slow
#test_strict(exclude_from_man = c("g_forest.Rd", "g_km.Rd", "t_forest_tte.Rd", "t_km.Rd"))
# may work by adding ", where = topenv(parent.frame())" to setClass in aa_trees.R, this is a bug
#test_strict(exclude_from_man = "aa_trees.Rd")
test_regexp()
