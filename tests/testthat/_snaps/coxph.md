# univariate works correctly

    Code
      res
    Output
      [1] "SEX"  "AGE"  "RACE"
      attr(,"varname")
      [1] "c(\"SEX\", \"AGE\", \"RACE\")"

---

    Code
      res2
    Output
      
      Call:
      lm(formula = SEX ~ univariate(ARM), data = tern_ex_adsl)
      
      Coefficients:
                        (Intercept)      univariate(ARM)B: Placebo  
                          1.4492754                      0.0027794  
      univariate(ARM)C: Combination  
                         -0.0009995  
      

# rht works correctly

    Code
      res
    Output
      [1] "+"     "m * x" "b"    

# estimate_coef works correctly

    Code
      res
    Output
                 coef se(coef) hr lcl ucl
      ARMCD/SEXM    0        0  1   1   1

---

    Code
      res
    Output
      [1] "Estimations of ARMCD hazard ratio given the level of SEX compared to ARMCD level ARM A."

# try_car_anova works correctly

    Code
      res
    Output
      [1] 1.0000000 1.0000000 0.9678110 0.3970066 0.3252267 0.5286392

---

    Code
      res
    Output
      NULL

# s_cox_multivariate works correctly with character input

    Code
      res
    Output
      Analysis of Deviance Table (Type III tests)
      
      Response: survival::Surv(time = AVAL, event = 1 - CNSR)
                 Df  Chisq Pr(>Chisq)
      ARMCD       2 1.1569     0.5608
      RACE        2 1.7917     0.4083
      AGE         1 0.0108     0.9174
      ARMCD:RACE  4 3.1853     0.5273
      ARMCD:AGE   2 1.1363     0.5666
      RACE:AGE    2 1.1686     0.5575

