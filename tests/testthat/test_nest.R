library(test.nest)

#test_lintr()
test_strict(exclude_from_man = c("g_forest.Rd", "g_km.Rd", "t_forest_tte.Rd", "t_km.Rd"))
test_regexp()
