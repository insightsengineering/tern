# s_count_nonmissing works with numeric input

    Code
      res
    Output
      $n
      [1] 10
      

# s_count_nonmissing also works with character input

    Code
      res
    Output
      $n
      [1] 4
      

# d_count_missed_doses works as expected

    Code
      res
    Output
      [1] "At least 1 missed dose"  "At least 5 missed doses"

# s_count_missed_doses works as expected

    Code
      res
    Output
      $n
      [1] 8
      
      $count_fraction
      $count_fraction$`2`
         count fraction 
           4.0      0.4 
      attr(,"label")
      [1] "At least 2 missed doses"
      
      $count_fraction$`5`
         count fraction 
             0        0 
      attr(,"label")
      [1] "At least 5 missed doses"
      
      

# count_missed_doses works as expected

    Code
      res
    Output
                                     A          B    
      ———————————————————————————————————————————————
      Missed Doses                                   
        n                            5          5    
        At least 3 missed doses   3 (60%)   5 (83.3%)
        At least 7 missed doses   2 (40%)   2 (33.3%)

# count_missed_doses works with denom argument specified

    Code
      res
    Output
                                       A         B    
      ————————————————————————————————————————————————
      x                                               
        Missed Doses                                  
          n                            5         1    
          At least 3 missed doses   3 (60%)   1 (100%)
          At least 7 missed doses   2 (40%)   1 (100%)
      y                                               
        Missed Doses                                  
          n                            0         4    
          At least 3 missed doses      0      4 (100%)
          At least 7 missed doses      0      1 (25%) 

