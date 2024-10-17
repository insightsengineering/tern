# or_glm estimates right OR and CI

    Code
      res
    Output
             est        lcl        ucl 
      0.25000000 0.00838718 7.45184921 

---

    Code
      res
    Output
             est        lcl        ucl 
      0.25000000 0.00838718 7.45184921 

# or_clogit estimates right OR and CI

    Code
      res
    Output
      $or_ci
      $or_ci$b
             est        lcl        ucl 
      0.28752892 0.03638778 2.27199590 
      
      $or_ci$c
             est        lcl        ucl 
      0.78025148 0.07473862 8.14561942 
      
      
      $n_tot
      n_tot 
         20 
      

# s_odds_ratio estimates right OR and CI (unstratified analysis)

    Code
      res
    Output
      $or_ci
             est        lcl        ucl 
      0.25000000 0.00838718 7.45184921 
      attr(,"label")
      [1] "Odds Ratio (95% CI)"
      
      $n_tot
      n_tot 
          6 
      attr(,"label")
      [1] "Total n"
      

# s_odds_ratio estimates right OR and CI (stratified analysis)

    Code
      res
    Output
      $or_ci
            est       lcl       ucl 
      0.7689750 0.3424155 1.7269154 
      attr(,"label")
      [1] "Odds Ratio (95% CI)"
      
      $n_tot
      n_tot 
        100 
      attr(,"label")
      [1] "Total n"
      

# estimate_odds_ratio estimates right OR and CI (unstratified analysis)

    Code
      res
    Output
                            a           b                     c         
      ——————————————————————————————————————————————————————————————————
      Odds Ratio (95% CI)       0.25 (0.01 - 7.45)   0.50 (0.02 - 11.09)

# estimate_odds_ratio estimates right OR and CI (stratified analysis)

    Code
      res
    Output
                            A           B         
      ————————————————————————————————————————————
      Odds Ratio (95% CI)       1.30 (0.58 - 2.92)

# estimate_odds_ratio works with strata and combined groups

    Code
      res
    Output
                            C: Combination   A: Drug X/B: Placebo
      ———————————————————————————————————————————————————————————
      Odds Ratio (95% CI)                     1.24 (0.54 - 2.89) 

# estimate_odds_ratio method argument works

    Code
      res
    Output
                                    A            B
      ————————————————————————————————————————————
      Odds Ratio (95% CI)   0.96 (0.85 - 1.08)    

