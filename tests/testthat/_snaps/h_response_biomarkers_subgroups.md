# h_rsp_to_logistic_variables works as expected

    Code
      res
    Output
      $response
      [1] "RSP"
      
      $arm
      [1] "AGE"
      
      $covariates
      [1] "A" "B"
      
      $strata
      [1] "D"
      

# h_logistic_mult_cont_df works as expected

    Code
      res
    Output
        biomarker              biomarker_label n_tot n_rsp prop        or       lcl
      1    BMRKR1 Continuous Level Biomarker 1   200   164 0.82 0.9748324 0.8795800
      2       AGE                          Age   200   164 0.82 0.9943862 0.9456819
             ucl conf_level      pval     pval_label
      1 1.080400       0.95 0.6270504 p-value (Wald)
      2 1.045599       0.95 0.8260959 p-value (Wald)

# h_logistic_mult_cont_df returns missing values if data is empty (0 rows)

    Code
      res
    Output
        biomarker              biomarker_label n_tot n_rsp prop or lcl ucl conf_level
      1    BMRKR1 Continuous Level Biomarker 1     0     0   NA NA  NA  NA       0.95
      2       AGE                          Age     0     0   NA NA  NA  NA       0.95
        pval     pval_label
      1   NA p-value (Wald)
      2   NA p-value (Wald)

# h_logistic_mult_cont_df also works with response not being called rsp

    Code
      res
    Output
        biomarker              biomarker_label n_tot n_rsp prop        or       lcl
      1    BMRKR1 Continuous Level Biomarker 1   200   164 0.82 0.9748324 0.8795800
      2       AGE                          Age   200   164 0.82 0.9943862 0.9456819
             ucl conf_level      pval     pval_label
      1 1.080400       0.95 0.6270504 p-value (Wald)
      2 1.045599       0.95 0.8260959 p-value (Wald)

