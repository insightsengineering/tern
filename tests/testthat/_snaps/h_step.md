# h_step_window works as expected for percentiles

    Code
      res
    Output
      [1] "sel"      "interval"

---

    Code
      res
    Output
      [1] "logical"

---

    Code
      res
    Output
      [1] "Percentile Center" "Percentile Lower"  "Percentile Upper" 
      [4] "Interval Center"   "Interval Lower"    "Interval Upper"   

---

    Code
      res
    Output
      [1] 5

# h_step_window works as expected for actual biomarker values

    Code
      res
    Output
      [1] "sel"      "interval"

---

    Code
      res
    Output
      [1] "Interval Center" "Interval Lower"  "Interval Upper" 

# h_step_trt_effect works for Cox models with interaction

    Code
      res
    Output
            se 
      1.322242 

# h_step_trt_effect works for logistic regression models with interaction

    Code
      res
    Output
             se 
      0.2918169 

# h_step_trt_effect works for conditional logistic regression with interaction

    Code
      res
    Output
             se 
      0.2906081 

# h_step_survival_est works as expected

    Code
      res
    Output
      [1] 3 6

---

    Code
      res
    Output
      [1] "n"        "events"   "loghr"    "se"       "ci_lower" "ci_upper"

---

    Code
      res
    Output
      [1] 1.075486 1.082507 1.089528

# h_step_rsp_est works as expected without strata

    Code
      res
    Output
      [1] 3 5

---

    Code
      res
    Output
      [1] "n"        "logor"    "se"       "ci_lower" "ci_upper"

---

    Code
      res
    Output
      [1] 2.00801169 1.02577315 0.04353461

---

    Code
      res
    Output
      [1] 8 8 8

# h_step_rsp_est works as expected with strata

    Code
      res
    Output
      [1] 3 5

---

    Code
      res
    Output
      [1] "n"        "logor"    "se"       "ci_lower" "ci_upper"

---

    Code
      res
    Output
      [1]  1.1340572  0.5111020 -0.1118531

---

    Code
      res
    Output
      [1] 8 8 8

