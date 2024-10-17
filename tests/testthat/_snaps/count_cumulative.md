# h_count_cumulative works with healthy input and default arguments

    Code
      res
    Output
          count  fraction 
      5.0000000 0.4545455 

# h_count_cumulative works with customized arguments

    Code
      res
    Output
          count  fraction 
      7.0000000 0.6363636 

# s_count_cumulative works with healthy input and default arguments

    Code
      res
    Output
      $count_fraction
      $count_fraction$`4`
          count  fraction 
      4.0000000 0.3636364 
      attr(,"label")
      [1] "<= 4"
      
      $count_fraction$`7`
          count  fraction 
      7.0000000 0.6363636 
      attr(,"label")
      [1] "<= 7"
      
      

# s_count_cumulative works with customized arguments

    Code
      res
    Output
      $count_fraction
      $count_fraction$`4`
          count  fraction 
      7.0000000 0.6363636 
      attr(,"label")
      [1] "> 4"
      
      $count_fraction$`7`
          count  fraction 
      5.0000000 0.4545455 
      attr(,"label")
      [1] "> 7"
      
      

# count_cumulative works with default arguments

    Code
      res
    Output
                  A          B    
      ————————————————————————————
      a                           
        <= 3   2 (40%)   1 (16.7%)
        <= 7   4 (80%)    3 (50%) 

# count_cumulative works with customized arguments

    Code
      res
    Output
                 A          B    
      ———————————————————————————
      a                          
        > 3   3 (60%)   4 (66.7%)
        > 7   1 (20%)   2 (33.3%)

# count_cumulative works with denom argument specified

    Code
      res
    Output
                   A         B    
      ————————————————————————————
      x            5         2    
        a                         
          > 3   3 (60%)   1 (100%)
          > 7   1 (20%)   1 (100%)
      y            0         4    
        a                         
          > 3      0      3 (75%) 
          > 7      0      1 (25%) 

