# h_coxreg_univar_formulas creates formulas with covariate

    Code
      res
    Output
                                             ref 
          "survival::Surv(time, status) ~ armcd" 
                                               X 
      "survival::Surv(time, status) ~ armcd + X" 
                                               y 
      "survival::Surv(time, status) ~ armcd + y" 

# h_coxreg_univar_formulas creates formulas with strata

    Code
      res
    Output
                                                            ref 
          "survival::Surv(time, status) ~ armcd + strata(SITE)" 
                                                              X 
      "survival::Surv(time, status) ~ armcd + X + strata(SITE)" 
                                                              y 
      "survival::Surv(time, status) ~ armcd + y + strata(SITE)" 

# h_coxreg_univar_formulas creates formula for reference when treatment is only considered

    Code
      res
    Output
                                         ref 
      "survival::Surv(time, status) ~ armcd" 

# h_coxreg_univar_formulas creates formulas with interactions

    Code
      res
    Output
                                                            ref 
          "survival::Surv(time, status) ~ armcd + strata(SITE)" 
                                                              X 
      "survival::Surv(time, status) ~ armcd * X + strata(SITE)" 
                                                              y 
      "survival::Surv(time, status) ~ armcd * y + strata(SITE)" 

# h_coxreg_univar_formulas creates formula without treatment arm

    Code
      res
    Output
                                                          X 
      "survival::Surv(time, status) ~ 1 + X + strata(SITE)" 
                                                          y 
      "survival::Surv(time, status) ~ 1 + y + strata(SITE)" 

# h_coxreg_univar_formulas creates formulas with multiple strata

    Code
      res
    Output
                                                                     ref 
          "survival::Surv(time, status) ~ armcd + strata(SITE, COUNTRY)" 
                                                                       X 
      "survival::Surv(time, status) ~ armcd + X + strata(SITE, COUNTRY)" 
                                                                       y 
      "survival::Surv(time, status) ~ armcd + y + strata(SITE, COUNTRY)" 

# h_coxreg_multivar_extract extracts correct coxph results when covariate names overlap

    Code
      res
    Output
             pval       hr       lcl      ucl level n term term_label
      1 0.3062027 1.029677 0.9735821 1.089003   age 8  age        age

# h_coxreg_multivar_extract extracts correct coxph results when covariate is a factor

    Code
      res
    Output
         term      pval            term_label       hr       lcl      ucl level  n
      1 stage        NA stage (reference = 1)       NA        NA       NA  <NA> NA
      2 stage 0.1947681                     2 4.989564 0.4394003 56.65848     2  8

# h_coxreg_multivar_formula creates formula without covariate

    Code
      res
    Output
      [1] "survival::Surv(TIME, EVNT) ~ ARMCD"

# h_coxreg_multivar_formula creates formulas with a strata

    Code
      res
    Output
      [1] "survival::Surv(time, status) ~ armcd + X + y + strata(SITE)"

# h_coxreg_multivar_formula creates formulas with multiple strata

    Code
      res
    Output
      [1] "survival::Surv(time, status) ~ armcd + X + y + strata(SITE, COUNTRY)"

# h_coxreg_multivar_formula creates formula with covariate

    Code
      res
    Output
      [1] "survival::Surv(time, status) ~ armcd + covar1 + covar2"

# h_coxreg_multivar_formula creates formula without treatment arm

    Code
      res
    Output
      [1] "survival::Surv(time, status) ~ 1 + covar1 + covar2"

# h_coxreg_multivar_formula creates formulas with multiple strata and without arm

    Code
      res
    Output
      [1] "survival::Surv(time, status) ~ 1 + X + y + strata(SITE, COUNTRY)"

# control_coxreg returns a standard list of parameters

    Code
      res
    Output
      $pval_method
      [1] "wald"
      
      $ties
      [1] "exact"
      
      $conf_level
      [1] 0.95
      
      $interaction
      [1] FALSE
      

# fit_coxreg_univar returns model results as expected

    Code
      res
    Output
      $mod
      $mod$ref
      Call:
      survival::coxph(formula = stats::as.formula(x), data = data, 
          ties = control$ties)
      
                coef exp(coef) se(coef)      z      p
      armcd2 -0.4484    0.6386   0.1990 -2.253 0.0242
      
      Likelihood ratio test=5.25  on 1 df, p=0.02191
      n= 340, number of events= 112 
      
      $mod$covar1
      Call:
      survival::coxph(formula = stats::as.formula(x), data = data, 
          ties = control$ties)
      
                 coef exp(coef) se(coef)      z        p
      armcd2  -0.4992    0.6070   0.2000 -2.496 0.012573
      covar12 -0.8180    0.4413   0.2393 -3.418 0.000631
      covar13 -1.1921    0.3036   0.2615 -4.559 5.13e-06
      covar14 -1.7264    0.1779   0.3074 -5.615 1.96e-08
      
      Likelihood ratio test=47.85  on 4 df, p=1.015e-09
      n= 340, number of events= 112 
      
      
      $data
          time status    arm armcd covar1 covar2 age
      1      1      0 ARM: 1     1      1      M  56
      2      1      0 ARM: 1     1      2      M  43
      3      1      0 ARM: 1     1      3      F  33
      4      1      0 ARM: 1     1      4      M  26
      5      4      0 ARM: 1     1      1      F  46
      6      4      0 ARM: 1     1      2      M  45
      7      4      0 ARM: 1     1      3      M  49
      8      4      0 ARM: 1     1      4      F  34
      9      7      0 ARM: 1     1      1      F  25
      10     7      0 ARM: 1     1      2      F  53
      11     7      0 ARM: 1     1      3      M  40
      12     7      0 ARM: 1     1      4      F  20
      13    10      0 ARM: 1     1      1      M  36
      14    10      0 ARM: 1     1      2      F  32
      15    10      0 ARM: 1     1      3      F  45
      16    10      0 ARM: 1     1      4      F  59
      17     6      1 ARM: 1     1      1      F  21
      18    10      0 ARM: 1     1      2      F  60
      19    10      0 ARM: 1     1      3      F  27
      20    10      0 ARM: 1     1      4      F  44
      21    14      0 ARM: 1     1      1      F  50
      22    14      0 ARM: 1     1      2      F  39
      23    14      0 ARM: 1     1      3      M  39
      24    14      0 ARM: 1     1      4      M  24
      25    18      0 ARM: 1     1      1      M  29
      26    18      0 ARM: 1     1      2      F  39
      27    18      0 ARM: 1     1      3      F  34
      28    18      0 ARM: 1     1      4      M  48
      29     5      1 ARM: 1     1      1      M  21
      30    18      0 ARM: 1     1      2      F  20
      31    18      0 ARM: 1     1      3      M  24
      32    18      0 ARM: 1     1      4      F  44
      33    12      1 ARM: 1     1      1      M  56
      34    16      1 ARM: 1     1      2      F  51
      35    18      0 ARM: 1     1      3      M  59
      36    18      0 ARM: 1     1      4      M  32
      37    23      0 ARM: 1     1      1      F  44
      38    23      0 ARM: 1     1      2      F  23
      39    23      0 ARM: 1     1      3      F  57
      40    23      0 ARM: 1     1      4      F  24
      41    10      1 ARM: 1     1      1      F  20
      42    15      1 ARM: 1     1      2      F  57
      43    23      0 ARM: 1     1      3      F  32
      44    23      0 ARM: 1     1      4      M  27
      45     3      1 ARM: 1     1      1      M  38
      46    16      1 ARM: 1     1      2      F  47
      47    23      1 ARM: 1     1      3      M  57
      48    23      0 ARM: 1     1      4      F  30
      49     3      1 ARM: 1     1      1      M  27
      50     9      1 ARM: 1     1      2      M  42
      51    21      1 ARM: 1     1      3      M  39
      52    23      0 ARM: 1     1      4      F  24
      53     7      1 ARM: 1     1      1      M  21
      54    10      1 ARM: 1     1      2      F  24
      55    16      1 ARM: 1     1      3      F  31
      56    24      1 ARM: 1     1      4      M  22
      57     3      1 ARM: 1     1      1      F  45
      58    15      1 ARM: 1     1      2      M  33
      59    25      1 ARM: 1     1      3      F  32
      60    25      0 ARM: 1     1      4      F  50
      61    26      0 ARM: 1     1      1      F  53
      62    26      0 ARM: 1     1      2      F  49
      63    26      0 ARM: 1     1      3      F  45
      64    26      0 ARM: 1     1      4      F  26
      65     1      1 ARM: 1     1      1      M  46
      66    26      0 ARM: 1     1      2      M  28
      67    26      0 ARM: 1     1      3      M  27
      68    26      0 ARM: 1     1      4      F  41
      69     2      1 ARM: 1     1      1      M  25
      70    26      1 ARM: 1     1      2      M  46
      71    26      0 ARM: 1     1      3      F  49
      72    26      0 ARM: 1     1      4      F  29
      73    25      1 ARM: 1     1      1      M  49
      74    28      0 ARM: 1     1      2      F  20
      75    28      0 ARM: 1     1      3      M  25
      76    28      0 ARM: 1     1      4      M  36
      77    29      0 ARM: 1     1      1      M  32
      78    29      0 ARM: 1     1      2      F  45
      79    29      0 ARM: 1     1      3      F  21
      80    29      0 ARM: 1     1      4      F  51
      81    29      0 ARM: 1     1      1      M  55
      82    29      0 ARM: 1     1      2      M  24
      83    29      0 ARM: 1     1      3      F  42
      84    29      0 ARM: 1     1      4      M  57
      85    29      0 ARM: 1     1      1      F  39
      86    29      0 ARM: 1     1      2      F  47
      87    29      0 ARM: 1     1      3      M  47
      88    29      0 ARM: 1     1      4      M  31
      89    28      1 ARM: 1     1      1      M  54
      90    30      1 ARM: 1     1      2      F  23
      91    30      0 ARM: 1     1      3      F  45
      92    30      0 ARM: 1     1      4      F  32
      93     2      1 ARM: 1     1      1      F  22
      94    17      1 ARM: 1     1      2      F  34
      95    22      1 ARM: 1     1      3      M  53
      96    30      0 ARM: 1     1      4      F  57
      97     3      1 ARM: 1     1      1      M  23
      98     6      1 ARM: 1     1      2      M  45
      99     8      1 ARM: 1     1      3      M  40
      100   12      1 ARM: 1     1      4      M  40
      101   12      1 ARM: 1     1      1      M  35
      102   15      1 ARM: 1     1      2      M  56
      103   24      1 ARM: 1     1      3      F  43
      104   31      0 ARM: 1     1      4      F  40
      105   32      0 ARM: 1     1      1      M  22
      106   32      0 ARM: 1     1      2      M  45
      107   32      0 ARM: 1     1      3      F  42
      108   32      0 ARM: 1     1      4      F  55
      109   34      0 ARM: 1     1      1      M  41
      110   34      0 ARM: 1     1      2      M  22
      111   34      0 ARM: 1     1      3      F  23
      112   34      0 ARM: 1     1      4      F  55
      113   36      0 ARM: 1     1      1      F  56
      114   36      0 ARM: 1     1      2      M  20
      115   36      0 ARM: 1     1      3      M  56
      116   36      0 ARM: 1     1      4      F  41
      117   29      1 ARM: 1     1      1      M  40
      118   36      0 ARM: 1     1      2      M  46
      119   36      0 ARM: 1     1      3      M  30
      120   36      0 ARM: 1     1      4      M  30
      121   37      0 ARM: 1     1      1      F  25
      122   37      0 ARM: 1     1      2      F  55
      123   37      0 ARM: 1     1      3      M  25
      124   37      0 ARM: 1     1      4      F  39
      125    9      1 ARM: 1     1      1      F  59
      126   17      1 ARM: 1     1      2      M  52
      127   22      1 ARM: 1     1      3      F  59
      128   24      1 ARM: 1     1      4      F  30
      129   16      1 ARM: 1     1      1      F  30
      130   19      1 ARM: 1     1      2      M  40
      131   23      1 ARM: 1     1      3      F  40
      132   29      1 ARM: 1     1      4      F  36
      133   41      0 ARM: 1     1      1      M  28
      134   41      0 ARM: 1     1      2      M  37
      135   41      0 ARM: 1     1      3      M  50
      136   41      0 ARM: 1     1      4      F  27
      137    3      1 ARM: 1     1      1      F  42
      138   43      0 ARM: 1     1      2      M  25
      139   43      0 ARM: 1     1      3      M  30
      140   43      0 ARM: 1     1      4      M  56
      141    6      1 ARM: 1     1      1      F  44
      142   43      0 ARM: 1     1      2      F  21
      143   43      0 ARM: 1     1      3      M  58
      144   43      0 ARM: 1     1      4      M  51
      145    3      1 ARM: 1     1      1      F  59
      146    6      1 ARM: 1     1      2      M  41
      147    9      1 ARM: 1     1      3      M  54
      148   44      0 ARM: 1     1      4      F  30
      149    9      1 ARM: 1     1      1      M  32
      150   11      1 ARM: 1     1      2      M  21
      151   20      1 ARM: 1     1      3      M  59
      152   26      1 ARM: 1     1      4      F  25
      153   18      1 ARM: 1     1      1      M  34
      154   48      0 ARM: 1     1      2      F  60
      155   48      0 ARM: 1     1      3      F  47
      156   48      0 ARM: 1     1      4      M  20
      157   49      0 ARM: 1     1      1      M  21
      158   49      0 ARM: 1     1      2      M  28
      159   49      0 ARM: 1     1      3      F  24
      160   49      0 ARM: 1     1      4      F  46
      161   35      1 ARM: 1     1      1      M  48
      162   51      0 ARM: 1     1      2      F  21
      163   51      0 ARM: 1     1      3      F  56
      164   51      0 ARM: 1     1      4      M  50
      165   17      1 ARM: 1     1      1      M  33
      166   53      0 ARM: 1     1      2      F  44
      167   53      0 ARM: 1     1      3      M  40
      168   53      0 ARM: 1     1      4      M  42
      169    3      1 ARM: 1     1      1      F  25
      170   15      1 ARM: 1     1      2      F  40
      171   46      1 ARM: 1     1      3      F  24
      172   51      1 ARM: 1     1      4      M  44
      173   59      0 ARM: 1     1      1      F  36
      174   59      0 ARM: 1     1      2      F  52
      175   59      0 ARM: 1     1      3      F  59
      176   59      0 ARM: 1     1      4      M  38
      177    2      1 ARM: 1     1      1      M  36
      178   15      1 ARM: 1     1      2      M  39
      179   24      1 ARM: 1     1      3      F  42
      180   30      1 ARM: 1     1      4      F  39
      181    5      1 ARM: 1     1      1      M  50
      182   14      1 ARM: 1     1      2      F  24
      183   19      1 ARM: 1     1      3      F  46
      184   27      1 ARM: 1     1      4      M  52
      185    2      1 ARM: 1     1      1      F  20
      186    8      1 ARM: 1     1      2      F  48
      187   12      1 ARM: 1     1      3      M  53
      188   13      1 ARM: 1     1      4      M  55
      189    1      0 ARM: 2     2      1      M  39
      190    1      0 ARM: 2     2      2      F  40
      191    1      0 ARM: 2     2      3      M  57
      192    1      0 ARM: 2     2      4      M  43
      193    1      0 ARM: 2     2      1      F  53
      194    1      0 ARM: 2     2      2      M  39
      195    1      0 ARM: 2     2      3      M  22
      196    1      0 ARM: 2     2      4      F  39
      197    5      1 ARM: 2     2      1      F  53
      198    5      0 ARM: 2     2      2      F  34
      199    5      0 ARM: 2     2      3      F  49
      200    5      0 ARM: 2     2      4      M  32
      201    9      0 ARM: 2     2      1      M  35
      202    9      0 ARM: 2     2      2      F  55
      203    9      0 ARM: 2     2      3      F  49
      204    9      0 ARM: 2     2      4      F  48
      205   10      0 ARM: 2     2      1      M  47
      206   10      0 ARM: 2     2      2      M  32
      207   10      0 ARM: 2     2      3      M  26
      208   10      0 ARM: 2     2      4      F  37
      209   13      0 ARM: 2     2      1      F  20
      210   13      0 ARM: 2     2      2      M  23
      211   13      0 ARM: 2     2      3      F  24
      212   13      0 ARM: 2     2      4      M  55
      213    3      1 ARM: 2     2      1      M  53
      214   14      0 ARM: 2     2      2      M  45
      215   14      0 ARM: 2     2      3      M  52
      216   14      0 ARM: 2     2      4      M  38
      217    1      1 ARM: 2     2      1      F  36
      218    3      1 ARM: 2     2      2      F  32
      219    5      1 ARM: 2     2      3      F  58
      220    7      1 ARM: 2     2      4      M  55
      221   18      0 ARM: 2     2      1      F  26
      222   18      0 ARM: 2     2      2      F  41
      223   18      0 ARM: 2     2      3      F  28
      224   18      0 ARM: 2     2      4      F  51
      225   17      1 ARM: 2     2      1      M  59
      226   18      0 ARM: 2     2      2      F  52
      227   18      0 ARM: 2     2      3      M  47
      228   18      0 ARM: 2     2      4      F  31
      229    2      1 ARM: 2     2      1      M  49
      230   19      0 ARM: 2     2      2      M  54
      231   19      0 ARM: 2     2      3      F  45
      232   19      0 ARM: 2     2      4      F  22
      233   17      1 ARM: 2     2      1      M  34
      234   19      1 ARM: 2     2      2      M  58
      235   21      0 ARM: 2     2      3      F  42
      236   21      0 ARM: 2     2      4      M  44
      237   22      0 ARM: 2     2      1      F  20
      238   22      0 ARM: 2     2      2      F  47
      239   22      0 ARM: 2     2      3      F  28
      240   22      0 ARM: 2     2      4      M  33
      241   25      0 ARM: 2     2      1      F  50
      242   25      0 ARM: 2     2      2      F  35
      243   25      0 ARM: 2     2      3      M  40
      244   25      0 ARM: 2     2      4      M  31
      245   25      0 ARM: 2     2      1      M  49
      246   25      0 ARM: 2     2      2      F  41
      247   25      0 ARM: 2     2      3      M  20
      248   25      0 ARM: 2     2      4      F  31
      249   25      0 ARM: 2     2      1      F  47
      250   25      0 ARM: 2     2      2      F  31
      251   25      0 ARM: 2     2      3      M  44
      252   25      0 ARM: 2     2      4      F  44
      253    6      1 ARM: 2     2      1      F  27
      254   12      1 ARM: 2     2      2      M  50
      255   13      1 ARM: 2     2      3      M  24
      256   26      0 ARM: 2     2      4      F  31
      257    6      1 ARM: 2     2      1      M  49
      258   27      0 ARM: 2     2      2      M  28
      259   27      0 ARM: 2     2      3      M  21
      260   27      0 ARM: 2     2      4      M  28
      261    2      1 ARM: 2     2      1      F  60
      262   29      0 ARM: 2     2      2      F  60
      263   29      0 ARM: 2     2      3      M  41
      264   29      0 ARM: 2     2      4      M  46
      265   26      1 ARM: 2     2      1      F  57
      266   35      1 ARM: 2     2      2      M  42
      267   36      0 ARM: 2     2      3      F  53
      268   36      0 ARM: 2     2      4      F  22
      269   38      0 ARM: 2     2      1      F  20
      270   38      0 ARM: 2     2      2      M  21
      271   38      0 ARM: 2     2      3      M  38
      272   38      0 ARM: 2     2      4      M  48
      273   22      1 ARM: 2     2      1      M  24
      274   23      1 ARM: 2     2      2      F  53
      275   27      1 ARM: 2     2      3      F  60
      276   32      1 ARM: 2     2      4      M  31
      277    4      1 ARM: 2     2      1      F  60
      278   16      1 ARM: 2     2      2      F  37
      279   23      1 ARM: 2     2      3      M  53
      280   27      1 ARM: 2     2      4      M  32
      281   24      1 ARM: 2     2      1      F  58
      282   26      1 ARM: 2     2      2      M  23
      283   29      1 ARM: 2     2      3      M  25
      284   40      1 ARM: 2     2      4      M  42
      285   41      0 ARM: 2     2      1      M  44
      286   41      0 ARM: 2     2      2      M  35
      287   41      0 ARM: 2     2      3      M  41
      288   41      0 ARM: 2     2      4      M  45
      289   41      0 ARM: 2     2      1      F  59
      290   41      0 ARM: 2     2      2      M  33
      291   41      0 ARM: 2     2      3      F  25
      292   41      0 ARM: 2     2      4      M  27
      293    1      1 ARM: 2     2      1      M  40
      294   27      1 ARM: 2     2      2      F  43
      295   43      0 ARM: 2     2      3      M  22
      296   43      0 ARM: 2     2      4      F  59
      297   44      0 ARM: 2     2      1      M  56
      298   44      0 ARM: 2     2      2      F  33
      299   44      0 ARM: 2     2      3      M  46
      300   44      0 ARM: 2     2      4      F  32
      301    2      1 ARM: 2     2      1      F  38
      302   20      1 ARM: 2     2      2      M  22
      303   23      1 ARM: 2     2      3      M  48
      304   27      1 ARM: 2     2      4      F  24
      305   45      0 ARM: 2     2      1      F  44
      306   45      0 ARM: 2     2      2      M  50
      307   45      0 ARM: 2     2      3      F  33
      308   45      0 ARM: 2     2      4      M  55
      309    2      1 ARM: 2     2      1      M  39
      310   46      0 ARM: 2     2      2      F  50
      311   46      0 ARM: 2     2      3      F  32
      312   46      0 ARM: 2     2      4      F  22
      313   46      0 ARM: 2     2      1      M  59
      314   46      0 ARM: 2     2      2      F  43
      315   46      0 ARM: 2     2      3      M  48
      316   46      0 ARM: 2     2      4      F  29
      317   49      0 ARM: 2     2      1      M  32
      318   49      0 ARM: 2     2      2      M  25
      319   49      0 ARM: 2     2      3      M  29
      320   49      0 ARM: 2     2      4      M  58
      321   50      0 ARM: 2     2      1      F  38
      322   50      0 ARM: 2     2      2      F  37
      323   50      0 ARM: 2     2      3      F  40
      324   50      0 ARM: 2     2      4      M  42
      325    4      1 ARM: 2     2      1      F  45
      326   24      1 ARM: 2     2      2      M  34
      327   47      1 ARM: 2     2      3      M  48
      328   50      0 ARM: 2     2      4      M  48
      329   54      0 ARM: 2     2      1      F  24
      330   54      0 ARM: 2     2      2      F  41
      331   54      0 ARM: 2     2      3      M  20
      332   54      0 ARM: 2     2      4      F  41
      333   38      1 ARM: 2     2      1      M  32
      334   54      0 ARM: 2     2      2      F  41
      335   54      0 ARM: 2     2      3      M  25
      336   54      0 ARM: 2     2      4      M  60
      337   59      0 ARM: 2     2      1      M  52
      338   59      0 ARM: 2     2      2      F  43
      339   59      0 ARM: 2     2      3      M  41
      340   59      0 ARM: 2     2      4      F  52
      
      $control
      $control$pval_method
      [1] "wald"
      
      $control$ties
      [1] "exact"
      
      $control$conf_level
      [1] 0.91
      
      $control$interaction
      [1] FALSE
      
      
      $vars
      $vars$time
      [1] "time"
      
      $vars$event
      [1] "status"
      
      $vars$arm
      [1] "armcd"
      
      $vars$covariates
      [1] "covar1"
      
      
      $at
      list()
      
      attr(,"class")
      [1] "coxreg.univar"

# fit_coxreg_univar works without treatment arm

    Code
      res
    Output
      [1] "age"

# tidy.summary.coxph method tidies up the Cox regression model

    Code
      res
    Output
         Pr(>|z|) exp(coef) exp(-coef) lower .95 upper .95  level n
      1 0.2472383  3.846606  0.2599694 0.3926671  37.68173 armcdB 8

# h_coxreg_univar_extract extracts coxph results

    Code
      res
    Output
            effect  term       term_label level n       hr       lcl      ucl
      1 Treatment: armcd B vs control (A)     B 8 3.846606 0.3926671 37.68173
             pval
      1 0.2472383

# tidy.coxreg.univar method tidies up the univariate Cox regression model

    Code
      res
    Output
                 effect   term        term_label level   n        hr       lcl
      ref    Treatment:  armcd  2 vs control (1)     2 340 0.6386426 0.4323844
      covar1 Covariate: covar1 A Covariate Label     2 340  0.607037 0.4101675
      covar2 Covariate: covar2         Sex (F/M)     2 340 0.6242738 0.4222423
                   ucl       pval                   ci
      ref    0.9432911 0.02423805 0.4323844, 0.9432911
      covar1 0.8983984 0.01257339 0.4101675, 0.8983984
      covar2 0.9229721 0.01818876 0.4222423, 0.9229721

# tidy.coxreg.univar method works with only numeric covariates with strata

    Code
      res
    Output
              effect  term       term_label level   n        hr       lcl       ucl
      ref Treatment: armcd 2 vs control (1)     2 340 0.6208343 0.4164994 0.9254162
      age Covariate:   age              age     2 340 0.6076894 0.4072018 0.9068879
                pval                   ci
      ref 0.01925561 0.4164994, 0.9254162
      age 0.01475084 0.4072018, 0.9068879

# tidy.coxreg.univar method works without treatment arm

    Code
      res
    Output
      [1] "age"               "covar1"            "A Covariate Label"
      [4] "A Covariate Label" "A Covariate Label"

# fit_coxreg_multivar returns model results as expected

    Code
      res
    Output
      Call:
      survival::coxph(formula = stats::as.formula(form), data = data, 
          ties = control$ties)
      
                 coef exp(coef) se(coef)      z        p
      armcd2  -0.5004    0.6063   0.2000 -2.503  0.01233
      covar12 -0.7842    0.4565   0.2407 -3.258  0.00112
      covar13 -1.1810    0.3070   0.2615 -4.516 6.29e-06
      covar14 -1.7054    0.1817   0.3078 -5.541 3.01e-08
      covar2M  0.2542    1.2894   0.1944  1.307  0.19105
      
      Likelihood ratio test=49.57  on 5 df, p=1.694e-09
      n= 340, number of events= 112 

# tidy.coxreg.multivar method tidies up the multivariate Cox regression model

    Code
      res
    Output
                            term         pval                        term_label
      armcd.1              armcd                            ARM (reference = 1)
      armcd.2                ARM   0.01274101                                 2
      covar1.1            covar1 7.121178e-09 A Covariate Label (reference = 1)
      covar1.2 A Covariate Label  0.001145167                                 2
      covar1.3 A Covariate Label 6.519833e-06                                 3
      covar1.4 A Covariate Label 3.296958e-08                                 4
      covar2.1            covar2                      Sex (F/M) (reference = F)
      covar2.2         Sex (F/M)    0.1979248                                 M
                      hr       lcl       ucl level                   ci
      armcd.1                   NA        NA  <NA>                     
      armcd.2  0.6106495 0.4142327 0.9002013     2 0.4142327, 0.9002013
      covar1.1                  NA        NA  <NA>                     
      covar1.2   0.46139 0.2894783 0.7353945     2 0.2894783, 0.7353945
      covar1.3 0.3111114 0.1872782 0.5168263     3 0.1872782, 0.5168263
      covar1.4 0.1847729 0.1015025 0.3363563     4 0.1015025, 0.3363563
      covar2.1                  NA        NA  <NA>                     
      covar2.2   1.28109 0.8786364 1.8678847     M 0.8786364, 1.8678847

