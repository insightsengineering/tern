# fit_survival_step works as expected with default options

    Code
      res
    Output
      [1] 12

---

    Code
      res
    Output
       [1] "Percentile Center" "Percentile Lower"  "Percentile Upper" 
       [4] "Interval Center"   "Interval Lower"    "Interval Upper"   
       [7] "n"                 "events"            "loghr"            
      [10] "se"                "ci_lower"          "ci_upper"         

# fit_survival_step works as expected with global model fit

    Code
      res
    Output
      [1] 9

---

    Code
      res
    Output
      [1] "Interval Center" "Interval Lower"  "Interval Upper"  "n"              
      [5] "events"          "loghr"           "se"              "ci_lower"       
      [9] "ci_upper"       

# fit_survival_step works as expected with null bandwidth

    Code
      res
    Output
      [1] 12

---

    Code
      res
    Output
       [1] "Percentile Center" "Percentile Lower"  "Percentile Upper" 
       [4] "Interval Center"   "Interval Lower"    "Interval Upper"   
       [7] "n"                 "events"            "loghr"            
      [10] "se"                "ci_lower"          "ci_upper"         

