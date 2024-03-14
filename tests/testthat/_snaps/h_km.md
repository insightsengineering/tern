# h_xticks works with default settings

    Code
      res
    Output
      [1]    0 1000 2000 3000 4000 5000

# h_xticks works with xticks number

    Code
      res
    Output
       [1]    0  100  200  300  400  500  600  700  800  900 1000 1100 1200 1300 1400
      [16] 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400 2500 2600 2700 2800 2900
      [31] 3000 3100 3200 3300 3400 3500 3600 3700 3800 3900 4000 4100 4200 4300 4400
      [46] 4500 4600 4700

# h_xticks works with xticks numeric

    Code
      res
    Output
      [1]    0  365 1000

# h_xticks works with max_time only

    Code
      res
    Output
      [1]    0 1000 2000 3000

# h_xticks works with xticks numeric when max_time is not NULL

    Code
      res
    Output
      [1]    0  365 1000

# h_xticks works with xticks number when max_time is not NULL

    Code
      res
    Output
      [1]    0  500 1000 1500

# h_tbl_median_surv estimates median survival time with CI

    Code
      res
    Output
             N Median        95% CI
      ARM A 69  974.6   (687, 1625)
      ARM B 73  727.8 (555.8, 1156)
      ARM C 58  632.3   (393, 1001)

# h_tbl_coxph_pairwise estimates HR, CI and pvalue

    Code
      res
    Output
              HR       95% CI p-value (log-rank)
      ARM B 1.41 (0.95, 2.09)             0.0905
      ARM C 1.81 (1.16, 2.84)             0.0086

---

    Code
      res
    Output
              HR       99% CI p-value (wald)
      ARM B 1.44 (0.85, 2.44)         0.0784
      ARM C 1.89 (1.03, 3.44)         0.0066

# h_data_plot works as expected

    Code
      res
    Output
      # A tibble: 203 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <fct> 
       1   0       70       0        0    1        0          1        1     ARM A 
       2  10.4     69       1        0    0.986    0.0146     1        0.958 ARM A 
       3  20.5     68       1        0    0.971    0.0208     1        0.932 ARM A 
       4  21.5     67       1        0    0.957    0.0257     1        0.910 ARM A 
       5  25.3     66       0        1    0.957    0.0257     1        0.910 ARM A 
       6  48.1     65       1        0    0.942    0.0300     0.999    0.888 ARM A 
       7  66.6     64       0        1    0.942    0.0300     0.999    0.888 ARM A 
       8  73.9     63       0        1    0.942    0.0300     0.999    0.888 ARM A 
       9  78.9     62       0        1    0.942    0.0300     0.999    0.888 ARM A 
      10  90.3     61       1        0    0.926    0.0342     0.991    0.866 ARM A 
      # i 193 more rows
      # i 1 more variable: censor <dbl>

# h_data_plot respects the ordering of the arm variable factor levels

    Code
      res
    Output
      [1] "ARM B" "ARM C" "ARM A"

# h_data_plot adds rows that have time 0 and estimate 1

    Code
      res
    Output
      [1] ARM A ARM B ARM C
      Levels: ARM A ARM B ARM C

