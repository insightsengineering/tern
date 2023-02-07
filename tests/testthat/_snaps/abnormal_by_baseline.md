# s_count_abnormal_by_baseline works with healthy input and default arguments

    Code
      res
    Output
      $fraction
      $fraction$not_abnormal
        num denom 
          1     3 
      attr(,"label")
      [1] "Not low baseline status"
      
      $fraction$abnormal
        num denom 
          0     1 
      attr(,"label")
      [1] "Low baseline status"
      
      $fraction$total
        num denom 
          1     4 
      attr(,"label")
      [1] "Total"
      
      

---

    Code
      res
    Output
      $fraction
      $fraction$not_abnormal
        num denom 
          1     3 
      attr(,"label")
      [1] "Not high baseline status"
      
      $fraction$abnormal
        num denom 
          1     1 
      attr(,"label")
      [1] "High baseline status"
      
      $fraction$total
        num denom 
          2     4 
      attr(,"label")
      [1] "Total"
      
      

# s_count_abnormal_by_baseline also works with tibble and custom arguments

    Code
      res
    Output
      $fraction
      $fraction$not_abnormal
        num denom 
          2     5 
      attr(,"label")
      [1] "Not low baseline status"
      
      $fraction$abnormal
        num denom 
          0     1 
      attr(,"label")
      [1] "Low baseline status"
      
      $fraction$total
        num denom 
          2     6 
      attr(,"label")
      [1] "Total"
      
      

---

    Code
      res
    Output
      $fraction
      $fraction$not_abnormal
        num denom 
          1     5 
      attr(,"label")
      [1] "Not high baseline status"
      
      $fraction$abnormal
        num denom 
          0     1 
      attr(,"label")
      [1] "High baseline status"
      
      $fraction$total
        num denom 
          1     6 
      attr(,"label")
      [1] "Total"
      
      

# count_abnormal_by_baseline throws warning with character var

    Code
      res
    Output
      $fraction
      $fraction$not_abnormal
        num denom 
          2     4 
      attr(,"label")
      [1] "Not low baseline status"
      
      $fraction$abnormal
        num denom 
          0     1 
      attr(,"label")
      [1] "Low baseline status"
      
      $fraction$total
        num denom 
          2     5 
      attr(,"label")
      [1] "Total"
      
      

# count_abnormal_by_baseline works with default arguments

    Code
      res
    Output
                                     all obs  
      ————————————————————————————————————————
      Low                                     
        Not low baseline status    1/3 (33.3%)
        Low baseline status            0/1    
        Total                       1/4 (25%) 
      High                                    
        Not high baseline status    1/2 (50%) 
        High baseline status        1/2 (50%) 
        Total                       2/4 (50%) 

# count_abnormal_by_baseline works with custom arguments

    Code
      res
    Output
                                   all obs
      ————————————————————————————————————
      Low                                 
        Not low baseline status     1 / 3 
        Low baseline status         0 / 1 
        Total                       1 / 4 
      High                                
        Not high baseline status    1 / 2 
        High baseline status        1 / 2 
        Total                       2 / 4 

