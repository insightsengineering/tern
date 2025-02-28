# h_surv_to_coxreg_variables works as expected

    Code
      res
    Output
      $time
      [1] "AVAL"
      
      $event
      [1] "EVNT"
      
      $arm
      [1] "AGE"
      
      $covariates
      [1] "A" "B"
      
      $strata
      [1] "D"
      

# h_coxreg_mult_cont_df works as expected

    Code
      res
    Output
        biomarker              biomarker_label n_tot n_tot_events   median       hr
      1    BMRKR1 Continuous Level Biomarker 1   200          141 753.5176 1.000735
      2       AGE                          Age   200          141 753.5176 1.008458
              lcl      ucl conf_level      pval     pval_label
      1 0.9541338 1.049611       0.95 0.9759230 p-value (Wald)
      2 0.9846171 1.032877       0.95 0.4901995 p-value (Wald)

# h_coxreg_mult_cont_df returns missing values if data is empty (0 rows)

    Code
      res
    Output
        biomarker              biomarker_label n_tot n_tot_events median hr lcl ucl
      1    BMRKR1 Continuous Level Biomarker 1     0            0     NA NA  NA  NA
      2       AGE                          Age     0            0     NA NA  NA  NA
        conf_level pval     pval_label
      1       0.95   NA p-value (Wald)
      2       0.95   NA p-value (Wald)

