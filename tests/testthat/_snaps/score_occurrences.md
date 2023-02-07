# score_occurrences functions as expected

    Code
      res
    Output
                                                                 A          B          C    
                                                               (N=3)      (N=1)      (N=1)  
      ——————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one event       2 (66.7%)   1 (100%)   1 (100%)
      Total number of events                                    10          5          5    
        AEBS1                                                                               
          Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
          Total number of events                                 7          3          4    
            AEPT1                                            2 (66.7%)   1 (100%)   1 (100%)
            AEPT2                                            2 (66.7%)   1 (100%)   1 (100%)
            AEPT3                                            1 (33.3%)   1 (100%)   1 (100%)
        AEBS2                                                                               
          Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
          Total number of events                                 3          2          1    
            AEPT2                                            2 (66.7%)   1 (100%)      0    
            AEPT1                                            1 (33.3%)      0          0    
            AEPT3                                                0          0       1 (100%)

# score_occurrences functions as expected with empty analysis rows

    Code
      res
    Output
                                                               A          B          C    
                                                             (N=3)      (N=1)      (N=1)  
      ————————————————————————————————————————————————————————————————————————————————————
      AEBS1                                                                               
        Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
        Total number of events                                 7          3          4    
          AEPT1                                            2 (66.7%)   1 (100%)   1 (100%)
          AEPT2                                            2 (66.7%)   1 (100%)   1 (100%)
          AEPT3                                            1 (33.3%)   1 (100%)   1 (100%)
      AEBS2                                                                               
        Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
        Total number of events                                 3          2          1    
          AEPT2                                            2 (66.7%)   1 (100%)      0    
          AEPT1                                            1 (33.3%)      0          0    
          AEPT3                                                0          0       1 (100%)
      EMPTY_LEVEL                                                                         
        Total number of patients with at least one event       0          0          0    
        Total number of events                                 0          0          0    

# score_occurrences_cols functions as expected

    Code
      res
    Output
                                                                 A          B          C    
                                                               (N=3)      (N=1)      (N=1)  
      ——————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one event       2 (66.7%)   1 (100%)   1 (100%)
      Total number of events                                    10          5          5    
        AEBS1                                                                               
          Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
          Total number of events                                 7          3          4    
            AEPT1                                            2 (66.7%)   1 (100%)   1 (100%)
            AEPT2                                            2 (66.7%)   1 (100%)   1 (100%)
            AEPT3                                            1 (33.3%)   1 (100%)   1 (100%)
        AEBS2                                                                               
          Total number of patients with at least one event   2 (66.7%)   1 (100%)   1 (100%)
          Total number of events                                 3          2          1    
            AEPT3                                                0          0       1 (100%)
            AEPT1                                            1 (33.3%)      0          0    
            AEPT2                                            2 (66.7%)   1 (100%)      0    

# score_occurrences_subtable functions as expected

    Code
      res
    Output
                    A          B          C    
                  (N=3)      (N=1)      (N=1)  
      —————————————————————————————————————————
      AEBS2                                    
        AEPT1   1 (33.3%)      0          0    
        AEPT2   2 (66.7%)   1 (100%)      0    
        AEPT3       0          0       1 (100%)
      AEBS1                                    
        AEPT1   2 (66.7%)   1 (100%)   1 (100%)
        AEPT2   2 (66.7%)   1 (100%)   1 (100%)
        AEPT3   1 (33.3%)   1 (100%)   1 (100%)

# score_occurrences_cont_cols functions as expected

    Code
      res
    Output
                                                         A           B          C    
      ———————————————————————————————————————————————————————————————————————————————
      AESS2                                                                          
        Number of patients with at least one event   7 (70.0%)   1 (20.0%)   5 (100%)
        Number of events                                 8           2          5    
        AESS2 (n)                                        7           1          5    
      AESS1                                                                          
        Number of patients with at least one event   2 (20.0%)   2 (40.0%)      0    
        Number of events                                 2           3          0    
        AESS1 (n)                                        2           2          0    

