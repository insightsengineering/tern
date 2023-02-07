# stat_mean_ci works for series without NAs (including extreme case n = 1 and various n_min values)

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
        y ymin ymax
      1 1   NA   NA

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
            -4.85        7.85 

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

# stat_mean_ci works for series with NAs (including extreme case n = 1 and various n_min values)

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
         y ymin ymax
      1 NA   NA   NA

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
         y ymin ymax
      1 NA   NA   NA

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
            -4.85        7.85 

---

    Code
      res
    Output
          y  ymin ymax
      1 1.5 -4.85 7.85

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

---

    Code
      res
    Output
      mean_ci_lwr mean_ci_upr 
               NA          NA 

# stat_mean_pval works for series without NAs (including extreme case n = 1 and various n_min values)

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
          0.2 

---

    Code
      res
    Output
      p_value 
           NA 

# stat_mean_pval works for series with NAs (including extreme case n = 1 and various n_min values)

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
          0.2 

---

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
           NA 

---

    Code
      res
    Output
      p_value 
           NA 

# stat_median_ci works for series without NAs (including extreme case n = 1)

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

---

    Code
      res
    Output
        y ymin ymax
      1 1   NA   NA

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                  2             9 
      attr(,"conf_level")
      [1] 0.9785156

# stat_median_ci works for series with NAs (including extreme case n = 1)

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

---

    Code
      res
    Output
         y ymin ymax
      1 NA   NA   NA

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

---

    Code
      res
    Output
         y ymin ymax
      1 NA   NA   NA

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                  2             9 
      attr(,"conf_level")
      [1] 0.9785156

---

    Code
      res
    Output
          y ymin ymax
      1 5.5    2    9

---

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

# stat_median_ci works for named numeric values when name is missing)

    Code
      res
    Output
      median_ci_lwr median_ci_upr 
                 NA            NA 
      attr(,"conf_level")
      [1] NA

