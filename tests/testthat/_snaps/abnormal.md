# s_count_abnormal works with healthy input and default arguments

    Code
      res
    Output
      $fraction
      $fraction$high
        num denom 
          1     2 
      attr(,"label")
      [1] "high"
      
      $fraction$low
        num denom 
          1     2 
      attr(,"label")
      [1] "low"
      
      

# s_count_abnormal works when excluding patients with abnormality at baseline

    Code
      res
    Output
      $fraction
      $fraction$high
        num denom 
          1     2 
      attr(,"label")
      [1] "high"
      
      $fraction$low
        num denom 
          1     2 
      attr(,"label")
      [1] "low"
      
      

# s_count_abnormal also works with tibble and custom arguments

    Code
      res
    Output
      $fraction
      $fraction$high
        num denom 
          0     1 
      attr(,"label")
      [1] "high"
      
      $fraction$low
        num denom 
          0     1 
      attr(,"label")
      [1] "low"
      
      

# count_abnormal works with default arguments

    Code
      res
    Output
              all obs 
      ————————————————
      low    1/2 (50%)
      high      0/1   

# count_abnormal works with custom arguments

    Code
      res
    Output
                all obs
      —————————————————
        < LLN    1 / 2 
        > ULN    0 / 1 

# count_abnormal works with default arguments and visit

    Code
      res
    Output
                all obs 
      ——————————————————
      WEEK 1            
        low    1/2 (50%)
        high   1/2 (50%)
      WEEK 2            
        low    1/2 (50%)
        high      0/2   

# s_count_abnormal works with healthy input and grouped abnormal arguments

    Code
      res
    Output
      $fraction
      $fraction$high
        num denom 
          2     4 
      attr(,"label")
      [1] "high"
      
      $fraction$low
        num denom 
          2     4 
      attr(,"label")
      [1] "low"
      
      

