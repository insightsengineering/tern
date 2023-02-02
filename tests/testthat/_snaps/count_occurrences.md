# s_count_occurrences functions as expected with valid input and default arguments

    Code
      res
    Output
      $count
      $count$MH1
      [1] 3
      
      $count$MH2
      [1] 1
      
      $count$MH3
      [1] 1
      
      
      $count_fraction
      $count_fraction$MH1
      [1] 3.00 0.75
      
      $count_fraction$MH2
      [1] 1.00 0.25
      
      $count_fraction$MH3
      [1] 1.00 0.25
      
      
      $fraction
      $fraction$MH1
        num denom 
          3     4 
      
      $fraction$MH2
        num denom 
          1     4 
      
      $fraction$MH3
        num denom 
          1     4 
      
      

# s_count_occurrences functions as expected when requesting different denominator

    Code
      res
    Output
      $count
      $count$MH1
      [1] 3
      
      $count$MH2
      [1] 1
      
      $count$MH3
      [1] 1
      
      
      $count_fraction
      $count_fraction$MH1
      [1] 3 1
      
      $count_fraction$MH2
      [1] 1.0000000 0.3333333
      
      $count_fraction$MH3
      [1] 1.0000000 0.3333333
      
      
      $fraction
      $fraction$MH1
        num denom 
          3     3 
      
      $fraction$MH2
        num denom 
          1     3 
      
      $fraction$MH3
        num denom 
          1     3 
      
      

# count_occurrences functions as expected with valid input and default arguments

    Code
      res
    Output
                A           B    
              (N=5)       (N=4)  
      ———————————————————————————
      MH1   3 (60.0%)   1 (25.0%)
      MH2   1 (20.0%)   2 (50.0%)
      MH3   1 (20.0%)   1 (25.0%)
      MH4       0       1 (25.0%)

# count_occurrences functions as expected with label row specified

    Code
      res
    Output
                 all obs 
      ———————————————————
      MH Term            
        MH1     4 (44.4%)
        MH2     3 (33.3%)

