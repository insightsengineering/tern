# tidy.step works as expected for survival STEP results

    Code
      res
    Output
       [1] "Percentile Center" "Percentile Lower"  "Percentile Upper" 
       [4] "Interval Center"   "Interval Lower"    "Interval Upper"   
       [7] "n"                 "events"            "Hazard Ratio"     
      [10] "se"                "ci_lower"          "ci_upper"         

---

    Code
      res
    Output
      [1] "biomarker" "ci"        "class"     "estimate"  "names"     "row.names"

# tidy.step works as expected for response STEP results

    Code
      res
    Output
       [1] "Percentile Center" "Percentile Lower"  "Percentile Upper" 
       [4] "Interval Center"   "Interval Lower"    "Interval Upper"   
       [7] "n"                 "Odds Ratio"        "se"               
      [10] "ci_lower"          "ci_upper"         

---

    Code
      res
    Output
      [1] "biomarker" "ci"        "class"     "estimate"  "names"     "row.names"

