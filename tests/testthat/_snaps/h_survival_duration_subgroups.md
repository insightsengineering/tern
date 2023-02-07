# h_survtime_df functions as expected with valid input and default arguments

    Code
      res
    Output
               arm  n n_events   median
      1 B: Placebo 73       57 727.8043
      2  A: Drug X 69       44 974.6402

# h_survtime_df functions as expected when median is NA

    Code
      res
    Output
        arm n n_events median
      1   A 3        1     NA
      2   B 3        3      2

# h_survtime_df functions as expected when 0 records in one group

    Code
      res
    Output
        arm n n_events median
      1   A 0       NA     NA
      2   B 6        4      5

# h_split_by_subgroups functions as expected with valid input and default arguments

    Code
      res
    Output
      $y.B
      $y.B$df
        x y z
      1 2 B D
      
      $y.B$df_labels
        subgroup var   var_label
      1        B   y label for y
      
      
      $y.A
      $y.A$df
        x y z
      1 1 A D
      
      $y.A$df_labels
        subgroup var   var_label
      1        A   y label for y
      
      
      $z.D
      $z.D$df
        x y z
      1 1 A D
      2 2 B D
      
      $z.D$df_labels
        subgroup var   var_label
      1        D   z label for z
      
      

# h_split_by_subgroups works as expected with groups_lists

    Code
      res
    Output
      [1] "y.AB" "z.D" 

---

    Code
      res
    Output
      $y.AB
      $y.AB$df
        x y z
      1 1 A D
      2 2 B D
      
      $y.AB$df_labels
        subgroup var   var_label
      1       AB   y label for y
      
      
      $z.D
      $z.D$df
        x y z
      1 1 A D
      2 2 B D
      
      $z.D$df_labels
        subgroup var   var_label
      1        D   z label for z
      
      

# h_survtime_subgroups_df functions as expected with valid input and default arguments

    Code
      res
    Output
                arm  n n_events    median     subgroup    var    var_label row_type
      1  B: Placebo 73       57  727.8043 All Patients    ALL All Patients  content
      2   A: Drug X 69       44  974.6402 All Patients    ALL All Patients  content
      3  B: Placebo 40       31  599.1772            F    SEX          SEX analysis
      4   A: Drug X 38       24 1016.2982            F    SEX          SEX analysis
      5  B: Placebo 33       26  888.4916            M    SEX          SEX analysis
      6   A: Drug X 31       20  974.6402            M    SEX          SEX analysis
      7  B: Placebo 24       21  735.4722          LOW BMRKR2       BMRKR2 analysis
      8   A: Drug X 26       15  974.6402          LOW BMRKR2       BMRKR2 analysis
      9  B: Placebo 23       14  731.8352       MEDIUM BMRKR2       BMRKR2 analysis
      10  A: Drug X 26       17  964.2197       MEDIUM BMRKR2       BMRKR2 analysis
      11 B: Placebo 26       22  654.8245         HIGH BMRKR2       BMRKR2 analysis
      12  A: Drug X 17       12 1016.2982         HIGH BMRKR2       BMRKR2 analysis

# h_survtime_subgroups_df functions as expected when subgroups is NULL.

    Code
      res
    Output
               arm  n n_events   median     subgroup var    var_label row_type
      1 B: Placebo 73       57 727.8043 All Patients ALL All Patients  content
      2  A: Drug X 69       44 974.6402 All Patients ALL All Patients  content

# h_survtime_subgroups_df works as expected with groups_lists

    Code
      res
    Output
      [1] "low"             "low"             "low/medium"      "low/medium"     
      [5] "low/medium/high" "low/medium/high"

# h_coxph_df functions as expected with valid input and default arguments

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl      ucl conf_level       pval
      1       142          101 0.7108557 0.4779138 1.057337       0.95 0.09049511
                pval_label
      1 p-value (log-rank)

# h_coxph_df functions as expected with one stratification factor

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl     ucl conf_level       pval
      1       142          101 0.6646586 0.4399495 1.00414       0.95 0.05089188
                pval_label
      1 p-value (log-rank)

# h_coxph_df functions as expected with multiple stratification factors

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
      1       142          101 0.6126133 0.3913507 0.9589739       0.95 0.03086774
                pval_label
      1 p-value (log-rank)

# h_coxph_df functions as expected when 0 records in one group

    Code
      res
    Output
        arm n_tot n_tot_events hr lcl ucl conf_level pval pval_label
      1        69           44 NA  NA  NA       0.95   NA         NA

# h_coxph_subgroups_df functions as expected with valid input and default arguments

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
      1       142          101 0.7108557 0.4779138 1.0573368       0.95 0.09049511
      2        78           55 0.5595391 0.3246658 0.9643271       0.95 0.03411759
      3        64           46 0.9102874 0.5032732 1.6464678       0.95 0.75582028
      4        50           36 0.7617717 0.3854349 1.5055617       0.95 0.43236030
      5        49           31 0.7651261 0.3641277 1.6077269       0.95 0.47860004
      6        43           34 0.6662356 0.3257413 1.3626456       0.95 0.26285846
                pval_label     subgroup    var    var_label row_type
      1 p-value (log-rank) All Patients    ALL All Patients  content
      2 p-value (log-rank)            F    SEX          SEX analysis
      3 p-value (log-rank)            M    SEX          SEX analysis
      4 p-value (log-rank)          LOW BMRKR2       BMRKR2 analysis
      5 p-value (log-rank)       MEDIUM BMRKR2       BMRKR2 analysis
      6 p-value (log-rank)         HIGH BMRKR2       BMRKR2 analysis

---

    Code
      res
    Output
        arm n_tot n_tot_events           hr lcl ucl conf_level      pval
      1         3            2 1.056023e-09   0 Inf       0.95 0.4795001
      2         1            0           NA  NA  NA       0.95        NA
      3         2            2           NA  NA  NA       0.95        NA
                pval_label     subgroup     var    var_label row_type
      1 p-value (log-rank) All Patients     ALL All Patients  content
      2               <NA>          GBR COUNTRY      COUNTRY analysis
      3               <NA>          CAN COUNTRY      COUNTRY analysis

# h_coxph_subgroups_df functions as expected with stratification factors

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl       ucl conf_level       pval
      1       142          101 0.6646586 0.4399495 1.0041404       0.95 0.05089188
      2        78           55 0.4824037 0.2671020 0.8712529       0.95 0.01372098
      3        64           46 0.8256627 0.4434121 1.5374387       0.95 0.54529396
                pval_label     subgroup var    var_label row_type
      1 p-value (log-rank) All Patients ALL All Patients  content
      2 p-value (log-rank)            F SEX          SEX analysis
      3 p-value (log-rank)            M SEX          SEX analysis

# h_coxph_subgroups_df functions as expected when subgroups is NULL.

    Code
      res
    Output
        arm n_tot n_tot_events        hr       lcl      ucl conf_level       pval
      1       142          101 0.7108557 0.4779138 1.057337       0.95 0.09049511
                pval_label     subgroup var    var_label row_type
      1 p-value (log-rank) All Patients ALL All Patients  content

# h_coxph_subgroups_df works as expected with groups_lists

    Code
      res
    Output
      [1] "low"             "low/medium"      "low/medium/high"

