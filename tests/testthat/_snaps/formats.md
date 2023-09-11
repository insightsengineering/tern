# format_fraction works with healthy inputs

    Code
      res
    Output
      [1] "2/3 (66.7%)"

# format_fraction works with 0 numerator input

    Code
      res
    Output
      [1] "0/3"

# format_fraction_fixed_dp works with healthy inputs

    Code
      res
    Output
      [1] "2/3 (66.7%)"

# format_fraction_fixed_dp works with whole number percentages

    Code
      res
    Output
      [1] "2/8 (25.0%)"

# format_fraction_fixed_dp works with 0 numerator input

    Code
      res
    Output
      [1] "0/3"

# format_count_fraction works with healthy inputs

    Code
      res
    Output
      [1] "2 (66.7%)"

# format_count_fraction works with count of 0

    Code
      res
    Output
      [1] "0"

# format_count_fraction_fixed_dp works with healthy inputs

    Code
      res
    Output
      [1] "2 (50.0%)"

---

    Code
      res
    Output
      [1] "2 (66.7%)"

# format_count_fraction_fixed_dp works with count of 0

    Code
      res
    Output
      [1] "0"

# format_xx works with easy inputs

    Code
      res
    Output
      [1] "2 (0.6)"    "10 (785.6)"

# format_sigfig works with easy inputs

    Code
      res
    Output
      [1] "1.66"    "0.576"   "0.100"   "78.6"    "0.00123"

# format_fraction_threshold works with easy inputs

    Code
      res
    Output
      [1] "10" "<2" "0" 

# h_get_format_threshold works with easy inputs

    Code
      res
    Output
      $threshold
         low   high 
        0.01 999.99 
      
      $format_string
            low      high 
        "<0.01" ">999.99" 
      

---

    Code
      res
    Output
      $threshold
        low  high 
        0.1 999.9 
      
      $format_string
           low     high 
        "<0.1" ">999.9" 
      

# h_format_threshold works with easy inputs

    Code
      res
    Output
      [1] "0.78"    "0.13"    ">999.99" "0.00"    "<0.01"   NA       

# format_extreme_values works with easy inputs

    Code
      res
    Output
      [1] "0.13"    ">999.99" "0.00"    "<0.01"   NA       

# format_extreme_values_ci works with easy inputs

    Code
      res
    Output
      [1] "(0.13, >999.99)" "(0.00, <0.01)"   "(NA, NA)"       

