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

# stat_propdiff_ci works with names and multiple values in x and y)

    Code
      res
    Output
      $A
      [1]  25.0000 -11.3387  61.3387
      
      $B
      [1] 70.00000 41.51302 98.48698
      
      $C
      [1] 50.00000 28.08694 71.91306
      

# stat_propdiff_ci works with custom arguments)

    Code
      res
    Output
      [[1]]
      [1] 0.38000000 0.01382145 0.74617855
      

