# AET09 variant 1 (AEs related to study drug by SMQ) is produced correctly

    Code
      res
    Output
                                                                                         A: Drug X    B: Placebo   C: Combination
                                                                                          (N=134)      (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event related to study drug     91 (67.9%)   90 (67.2%)     93 (70.5%)  
      SMQ1                                                                                                                       
        Total number of patients with at least one adverse event related to study drug   91 (67.9%)   90 (67.2%)     93 (70.5%)  
        Total number of events related to study drug                                        172          174            197      
        dcd D.1.1.1.1                                                                    50 (37.3%)   42 (31.3%)     51 (38.6%)  
        dcd B.1.1.1.1                                                                    47 (35.1%)   49 (36.6%)     43 (32.6%)  
        dcd C.1.1.1.3                                                                    43 (32.1%)   46 (34.3%)     43 (32.6%)  

# AET09 variant 2 (AEs related to study drug by SMQ with customized queries) is produced correctly

    Code
      res
    Output
                                                                                         A: Drug X    B: Placebo   C: Combination
                                                                                          (N=134)      (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event related to study drug     99 (73.9%)   98 (73.1%)    102 (77.3%)  
      SMQ1                                                                                                                       
        Total number of patients with at least one adverse event related to study drug   91 (67.9%)   90 (67.2%)     93 (70.5%)  
        Total number of events related to study drug                                        172          174            197      
        dcd D.1.1.1.1                                                                    50 (37.3%)   42 (31.3%)     51 (38.6%)  
        dcd B.1.1.1.1                                                                    47 (35.1%)   49 (36.6%)     43 (32.6%)  
        dcd C.1.1.1.3                                                                    43 (32.1%)   46 (34.3%)     43 (32.6%)  
      SMQ2                                                                                                                       
        Total number of patients with at least one adverse event related to study drug   50 (37.3%)   42 (31.3%)     51 (38.6%)  
        Total number of events related to study drug                                         61           51             71      
        dcd D.1.1.1.1                                                                    50 (37.3%)   42 (31.3%)     51 (38.6%)  
      SMQ3                                                                                                                       
        Total number of patients with at least one adverse event related to study drug   35 (26.1%)   48 (35.8%)     55 (41.7%)  
        Total number of events related to study drug                                         48           53             65      
        dcd C.2.1.2.1                                                                    35 (26.1%)   48 (35.8%)     55 (41.7%)  

