# `prop_diff_ha` (proportion difference by Anderson-Hauck)

    Code
      res
    Output
      $diff
      [1] 0.25
      
      $diff_ci
      [1] -0.9195011  1.0000000
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] -0.8451161  0.8451161
      

# `prop_diff_nc` (proportion difference by Newcombe)

    Code
      res
    Output
      $diff
      [1] 0.25
      
      $diff_ci
      [1] -0.2966681  0.6750199
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] -0.361619  0.361619
      

# `prop_diff_wald` (proportion difference by Wald's test: with correction)

    Code
      res
    Output
      $diff
      [1] 0.25
      
      $diff_ci
      [1] -0.8069203  1.0000000
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] -0.9208106  0.9208106
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] -0.375  0.375
      

# `prop_diff_wald` (proportion difference by Wald's test: without correction)

    Code
      res
    Output
      $diff
      [1] 0.25
      
      $diff_ci
      [1] -0.4319203  0.9319203
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] -0.4208106  0.4208106
      

---

    Code
      res
    Output
      $diff
      [1] 0
      
      $diff_ci
      [1] 0 0
      

# `prop_diff_cmh` (proportion difference by CMH)

    Code
      res
    Output
      $prop
        Placebo Treatment 
      0.5331117 0.3954251 
      
      $prop_ci
      $prop_ci$Placebo
      [1] 0.4306536 0.6355698
      
      $prop_ci$Treatment
      [1] 0.2890735 0.5017768
      
      
      $diff
      [1] -0.1376866
      
      $diff_ci
      [1] -0.285363076  0.009989872
      
      $weights
            a.x       b.x       a.y       b.y       a.z       b.z 
      0.1148388 0.2131696 0.1148388 0.2131696 0.1767914 0.1671918 
      
      $n1
      a.x b.x a.y b.y a.z b.z 
        4  11   8  11  13  11 
      
      $n2
      a.x b.x a.y b.y a.z b.z 
        8   9   4   9   6   6 
      

# prop_diff_cmh works correctly when some strata don't have both groups

    Code
      res
    Output
      $prop
        Placebo Treatment 
       0.569842  0.398075 
      
      $prop_ci
      $prop_ci$Placebo
      [1] 0.4637119 0.6759721
      
      $prop_ci$Treatment
      [1] 0.2836122 0.5125378
      
      
      $diff
      [1] -0.171767
      
      $diff_ci
      [1] -0.32786094 -0.01567301
      
      $weights
            b.x       a.y       b.y       a.z       b.z 
      0.2408257 0.1297378 0.2408257 0.1997279 0.1888829 
      
      $n1
      b.x a.y b.y a.z b.z 
       11   8  11  13  11 
      
      $n2
      b.x a.y b.y a.z b.z 
        9   4   9   6   6 
      

# prop_diff_strat_nc output matches equivalent SAS function output

    Code
      res
    Output
           value      lower      upper 
      0.25390590 0.03467969 0.44544132 

# `estimate_proportion_diff` is compatible with `rtables`

    Code
      res
    Output
                                        B         A       
      ————————————————————————————————————————————————————
      Difference in Response rate (%)            25.0     
        90% CI (Anderson-Hauck)             (-92.0, 100.0)

# `estimate_proportion_diff` and cmh is compatible with `rtables`

    Code
      res
    Output
                                           B            A         
      ————————————————————————————————————————————————————————————
      Difference in Response rate (%)                -4.2133      
        90% CI (CMH, without correction)       (-20.0215, 11.5950)

# s_proportion_diff works with no strata

    Code
      res
    Output
      $diff
       diff_ha 
      14.69622 
      attr(,"label")
      [1] "Difference in Response rate (%)"
      
      $diff_ci
      diff_ci_ha_l diff_ci_ha_u 
         -3.118966    32.511412 
      attr(,"label")
      [1] "90% CI (Anderson-Hauck)"
      

# s_proportion_diff works with strata

    Code
      res
    Output
      $diff
      diff_cmh 
      13.76866 
      attr(,"label")
      [1] "Difference in Response rate (%)"
      
      $diff_ci
      diff_ci_cmh_l diff_ci_cmh_u 
         -0.9989872    28.5363076 
      attr(,"label")
      [1] "90% CI (CMH, without correction)"
      

