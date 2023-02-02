# s_count_values works for character input without NAs

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0000000 0.6666667
      
      $n_blq
      [1] 0
      

---

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 0
      
      $count_fraction
      [1] 0 0
      
      $n_blq
      [1] 0
      

# s_count_values works for character input with NAs

    Code
      res
    Output
      $n
      [1] 5
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0 0.4
      
      $n_blq
      [1] 0
      

---

    Code
      res
    Output
      $n
      [1] 6
      
      $count
      [1] 1
      
      $count_fraction
      [1] 1.0000000 0.1666667
      
      $n_blq
      [1] 0
      

# s_count_values can pass options to s_summary's logical method

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0 0.2
      
      $n_blq
      [1] 0
      

# s_count_values for factor gives same result as for character

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0 0.2
      
      $n_blq
      [1] 0
      

# s_count_values for factor gives the same result as for character for values not in factor level

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 0
      
      $count_fraction
      [1] 0 0
      
      $n_blq
      [1] 0
      

# count_values works as expected with a single value

    Code
      res
    Output
                 all obs  
      ————————————————————
      setosa   50 (33.33%)

# count_values works as expected with multiple values and variables

    Code
      res
    Output
                all obs  
      ———————————————————
      x                  
        a, f   2 (50.00%)
      y                  
        a, f   3 (75.00%)

---

    Code
      res
    Output
              all obs  
      —————————————————
      TRUE   2 (50.00%)

# s_count_values for logical vector

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0000000 0.6666667
      
      $n_blq
      [1] 0
      

# s_count_values for logical vector with NA

    Code
      res
    Output
      $n
      [1] 3
      
      $count
      [1] 2
      
      $count_fraction
      [1] 2.0000000 0.6666667
      
      $n_blq
      [1] 0
      

