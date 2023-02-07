# s_count_abnormal_by_worst_grade works as expected

    Code
      res
    Output
      $count_fraction
      $count_fraction$`1`
          count  fraction 
      12.000000  0.173913 
      attr(,"label")
      [1] "1"
      
      $count_fraction$`2`
          count  fraction 
      9.0000000 0.1304348 
      attr(,"label")
      [1] "2"
      
      $count_fraction$`3`
           count   fraction 
      6.00000000 0.08695652 
      attr(,"label")
      [1] "3"
      
      $count_fraction$`4`
          count  fraction 
      7.0000000 0.1014493 
      attr(,"label")
      [1] "4"
      
      $count_fraction$Any
           count   fraction 
      34.0000000  0.4927536 
      attr(,"label")
      [1] "Any"
      
      

# count_abnormal_by_worst_grade works as expected

    Code
      res
    Output
                                       ARM A        ARM B        ARM C   
      ———————————————————————————————————————————————————————————————————
      Immunoglobulin A Measurement                                       
        HIGH                                                             
          1                          7 (10.1%)     7 (9.6%)    6 (10.3%) 
          2                          8 (11.6%)     6 (8.2%)    8 (13.8%) 
          3                          7 (10.1%)     5 (6.8%)    9 (15.5%) 
          4                           6 (8.7%)     2 (2.7%)     3 (5.2%) 
          Any                        28 (40.6%)   20 (27.4%)   26 (44.8%)

