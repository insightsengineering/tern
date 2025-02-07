# h_adlb_worsen stacks data correctly (simple case)

    Code
      res
    Output
         USUBJID PARAMCD       VALUES         MIN      MAX WGRLOFL WGRHIFL GRADDR
      1        4     ABC   0.04777932  0.04777932 186.6068       Y            Low
      2        3     OPQ   0.31411084  0.31411084 143.8712       Y            Low
      3        4     OPQ   0.45459322  0.45459322 192.5216       Y            Low
      4        2     ABC   0.78966776  0.78966776 181.3203       Y            Low
      5        2     OPQ   7.48620657  7.48620657 194.7080       Y            Low
      6        5     OPQ  22.74372182 22.74372182 187.4772       Y            Low
      7        1     OPQ  23.49747233 23.49747233 195.6453       Y            Low
      8        1     ABC  26.93331945 26.93331945 187.4151       Y            Low
      9        3     ABC  37.89478708 37.89478708 196.5634       Y            Low
      10       5     ABC  46.70470511 46.70470511 192.5141       Y            Low
      11       3     OPQ 143.87116753  0.31411084 143.8712               Y   High
      12       3     XYZ 155.16467253 17.12241299 155.1647               Y   High
      13       2     XYZ 156.93855514  7.78729822 156.9386               Y   High
      14       4     XYZ 167.36031189  0.27616872 167.3603               Y   High
      15       5     OPQ 187.47716993 22.74372182 187.4772               Y   High
      16       5     XYZ 188.94406511 16.05289332 188.9441               Y   High
      17       4     OPQ 192.52160275  0.45459322 192.5216               Y   High
      18       2     OPQ 194.70798275  7.48620657 194.7080               Y   High
      19       1     OPQ 195.64528568 23.49747233 195.6453               Y   High
      20       1     XYZ 197.77834578 16.48751162 197.7783               Y   High

# h_adlb_worsen stacks data correctly

    Code
      res
    Output
                     USUBJID ARMCD        AVISIT PARAMCD ATOXGR BTOXGR WGRLOFL
      1  AB12345-CHN-1-id-53 ARM B  WEEK 1 DAY 8     IGA      0      0        
      2 AB12345-CHN-3-id-128 ARM B WEEK 5 DAY 36     IGA      4      0        
      3  AB12345-CHN-1-id-53 ARM B WEEK 3 DAY 22     ALT      4      0        
      4 AB12345-CHN-3-id-128 ARM B WEEK 4 DAY 29     ALT      1      0        
      5  AB12345-CHN-1-id-53 ARM B WEEK 3 DAY 22     CRP      0      1       Y
      6 AB12345-CHN-3-id-128 ARM B  WEEK 1 DAY 8     CRP      0      0       Y
      7  AB12345-CHN-1-id-53 ARM B WEEK 5 DAY 36     ALT     -4      0       Y
      8 AB12345-CHN-3-id-128 ARM B WEEK 5 DAY 36     ALT     -4      0       Y
        WGRHIFL GRADDR
      1       Y   High
      2       Y   High
      3       Y   High
      4       Y   High
      5       Y    Low
      6            Low
      7            Low
      8            Low

# h_worsen_counter counts data (low) correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# h_worsen_counter counts data (high) correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# h_worsen_counter counts data (low), no high correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          2    10 
      
      $fraction$`2`
        num denom 
          3    15 
      
      $fraction$`3`
        num denom 
          4    20 
      
      $fraction$`4`
        num denom 
          5    25 
      
      $fraction$Any
        num denom 
         14    25 
      
      

# h_worsen_counter counts data (low), no low correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          0    30 
      
      $fraction$`2`
        num denom 
          0    30 
      
      $fraction$`3`
        num denom 
          0    30 
      
      $fraction$`4`
        num denom 
          0    30 
      
      $fraction$Any
        num denom 
          0    30 
      
      

# s_count_abnormal_lab_worsen_by_baseline

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# count_abnormal_lab_worsen_by_baseline

    Code
      res
    Output
                    ARM A           ARM B           ARM C    
                   (N=69)          (N=73)          (N=58)    
      ———————————————————————————————————————————————————————
      IGA                                                    
        High                                                 
          1      6/63 (9.5%)     6/64 (9.4%)      4/50 (8%)  
          2     8/64 (12.5%)     5/67 (7.5%)    8/53 (15.1%) 
          3     7/66 (10.6%)     5/68 (7.4%)    9/57 (15.8%) 
          4      6/68 (8.8%)     2/72 (2.8%)     3/58 (5.2%) 
          Any   27/68 (39.7%)    18/72 (25%)    24/58 (41.4%)
      ALT                                                    
        High                                                 
          1     7/63 (11.1%)     6/62 (9.7%)     2/48 (4.2%) 
          2      12/63 (19%)      4/67 (6%)      11/50 (22%) 
          3      4/65 (6.2%)    11/71 (15.5%)   7/56 (12.5%) 
          4      1/67 (1.5%)    8/71 (11.3%)      4/57 (7%)  
          Any   24/67 (35.8%)   29/71 (40.8%)   24/57 (42.1%)
        Low                                                  
          1     12/67 (17.9%)    4/66 (6.1%)    7/52 (13.5%) 
          2     9/68 (13.2%)    12/69 (17.4%)   6/55 (10.9%) 
          3      6/69 (8.7%)     4/71 (5.6%)     5/56 (8.9%) 
          4     7/69 (10.1%)     7/73 (9.6%)    6/58 (10.3%) 
          Any   34/69 (49.3%)    27/73 (37%)    24/58 (41.4%)
      CRP                                                    
        Low                                                  
          1     11/66 (16.7%)   10/67 (14.9%)    4/47 (8.5%) 
          2     8/66 (12.1%)     1/70 (1.4%)     6/50 (12%)  
          3      4/68 (5.9%)    9/70 (12.9%)     5/53 (9.4%) 
          4     7/69 (10.1%)     6/72 (8.3%)     4/55 (7.3%) 
          Any   30/69 (43.5%)   26/72 (36.1%)   19/55 (34.5%)

