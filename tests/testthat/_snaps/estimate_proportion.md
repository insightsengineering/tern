# prop_wilson returns right result

    Code
      res
    Output
      [1] 0.2692718 0.7307282

# strata_normal_quantile works with general factor table

    Code
      res
    Output
      [1] 1.133272

# update_weights_strat_wilson works with general inputs

    Code
      res
    Output
      $n_it
      [1] 3
      
      $weights
      [1] 0.2067191 0.1757727 0.1896962 0.1636346 0.1357615 0.1284160
      
      $diff_v
      [1] 1.458717e-01 1.497223e-03 1.442189e-06
      

# update_weights_strat_wilson convergence test

    Code
      res
    Output
      [1] 2

# prop_strat_wilson returns right results

    Code
      res
    Output
      $conf_int
          lower     upper 
      0.4072891 0.5647887 
      
      $weights
            a.x       b.x       a.y       b.y       a.z       b.z 
      0.2074199 0.1776464 0.1915610 0.1604678 0.1351096 0.1277952 
      

# prop_strat_wilson returns right result with inserted weights

    Code
      res
    Output
      $conf_int
          lower     upper 
      0.4190436 0.5789733 
      

# prop_clopper_pearson returns right result

    Code
      res
    Output
      [1] 0.187086 0.812914

# prop_wald returns right result

    Code
      res
    Output
      [1] 0.1401025 0.8598975

---

    Code
      res
    Output
      [1] 0.1901025 0.8098975

# prop_agresti_coull returns right result

    Code
      res
    Output
      [1] 0.2365931 0.7634069

# prop_jeffreys returns right result

    Code
      res
    Output
      [1] 0.2235287 0.7764713

# prop_strat_wilson output matches equivalent SAS function output

    Code
      res
    Output
          lower     upper 
      0.4867191 0.7186381 

# s_proportion returns right result

    Code
      res
    Output
      $n_prop
      [1] 2.0 0.5
      attr(,"label")
      [1] "Responders"
      
      $prop_ci
      [1]   0 100
      attr(,"label")
      [1] "95% CI (Wald, with correction)"
      

# `s_proportion` works with Jeffreys CI

    Code
      res
    Output
      $n_prop
      [1] 4.0000000 0.6666667
      attr(,"label")
      [1] "Responders"
      
      $prop_ci
      [1] 34.08020 89.57295
      attr(,"label")
      [1] "90% CI (Jeffreys)"
      

---

    Code
      res
    Output
      $n_prop
      [1] 4 1
      attr(,"label")
      [1] "Responders"
      
      $prop_ci
      [1]  55.52374 100.00000
      attr(,"label")
      [1] "95% CI (Jeffreys)"
      

# `s_proportion` works with Agresti-Coull CI

    Code
      res
    Output
      $n_prop
      [1] 4.0000000 0.6666667
      attr(,"label")
      [1] "Responders"
      
      $prop_ci
      [1] 34.35850 88.61537
      attr(,"label")
      [1] "90% CI (Agresti-Coull)"
      

---

    Code
      res
    Output
      $n_prop
      [1] 4 1
      attr(,"label")
      [1] "Responders"
      
      $prop_ci
      [1]  45.40497 100.00000
      attr(,"label")
      [1] "95% CI (Agresti-Coull)"
      

# `estimate_proportion` is compatible with `rtables`

    Code
      res
    Output
           [,1]                 [,2]                 [,3]                 
      [1,] "68.00 (98.55%)"     "68.00 (93.15%)"     "58.00 (100.00%)"    
      [2,] "(92.2369, 99.7437)" "(84.9479, 97.0391)" "(93.7882, 100.0000)"
           [,4]                
      [1,] "194.00 (97.00%)"   
      [2,] "(93.6106, 98.6180)"

# `estimate_proportion` and strat_wilson is compatible with `rtables`

    Code
      res
    Output
           [,1]                 [,2]                 [,3]                
      [1,] "39.00 (56.52%)"     "38.00 (52.05%)"     "31.00 (53.45%)"    
      [2,] "(37.1236, 63.4557)" "(35.1170, 57.1709)" "(33.3127, 57.6543)"
           [,4]                
      [1,] "108.00 (54.00%)"   
      [2,] "(46.8585, 60.2254)"

# `estimate_proportion` and strat_wilson with equal weights and specific number of interactions works with `rtables`

    Code
      res
    Output
           [,1]                 [,2]                 [,3]                
      [1,] "39.00 (56.52%)"     "38.00 (52.05%)"     "31.00 (53.45%)"    
      [2,] "(45.3208, 67.3762)" "(39.3366, 61.9179)" "(41.6771, 65.7415)"
           [,4]                
      [1,] "108.00 (54.00%)"   
      [2,] "(46.7669, 60.5951)"

