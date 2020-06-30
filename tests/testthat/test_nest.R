library(test.nest)

test_all(strict_args = list(exclude_from_man = c("decorate_grob.Rd", #partial argument matches
                                                 "g_forest.Rd",
                                                 "g_km.Rd",
                                                 "s_coxph_pairwise.Rd",
                                                 "s_logistic_interaction.Rd",
                                                 "s_logistic_single.Rd",
                                                 "t_coxph_pairwise.Rd",
                                                 "t_el_disposition.Rd",
                                                 "t_el_forest_tte.Rd",
                                                 "t_el_odds_ratio.Rd",
                                                 "t_forest_rsp.Rd",
                                                 "t_forest_tte.Rd",
                                                 "t_km.Rd",
                                                 "t_tte.Rd",
                                                 "s_odds_ratio.Rd")))
