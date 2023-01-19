# EXT01 default variant with numeric parameters is produced correctly

    Code
      res
    Output
                                                     A: Drug X        B: Placebo      C: Combination 
                                                      (N=134)           (N=134)           (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————
      Total dose administered                                                                        
        n                                               75                67                75       
        Mean (SD)                                 6675.2 (1110.9)   6505.1 (1249.3)   6982.4 (1272.5)
        Median                                        6720.0            6480.0            7200.0     
        Min - Max                                 4800.0 - 9360.0   4080.0 - 9360.0   4320.0 - 9360.0
      Total number of doses administered                                                             
        n                                               75                67                75       
        Mean (SD)                                    7.0 (0.0)         7.0 (0.0)         7.0 (0.0)   
        Median                                          7.0               7.0               7.0      
        Min - Max                                    7.0 - 7.0         7.0 - 7.0         7.0 - 7.0   
      Treatment duration (days)                                                                      
        n                                               75                67                75       
        Mean (SD)                                   74.3 (41.6)       79.0 (43.1)       74.2 (39.5)  
        Median                                         77.0              80.0              78.0      
        Min - Max                                   5.0 - 149.0       2.0 - 150.0       1.0 - 147.0  
      Total number of missed doses during study                                                      
        n                                               75                67                75       
        Mean (SD)                                   10.5 (5.9)        10.0 (6.1)         9.5 (5.5)   
        Median                                         10.0              11.0               9.0      
        Min - Max                                   0.0 - 20.0        0.0 - 19.0        0.0 - 20.0   

# EXT01 variant: with both numeric and categorical parameters

    Code
      res
    Output
                                              A: Drug X        B: Placebo      C: Combination 
                                               (N=134)           (N=134)           (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————
      Treatment duration (days)                                                               
        n                                        75                67                75       
        Mean (SD)                            74.3 (41.6)       79.0 (43.1)       74.2 (39.5)  
        Median                                  77.0              80.0              78.0      
        Min - Max                            5.0 - 149.0       2.0 - 150.0       1.0 - 147.0  
      Treatment duration (days)                                                               
        n                                        75                67                75       
        0 - 30                                12 (16%)         12 (17.9%)         15 (20%)    
        31 - 60                               18 (24%)         12 (17.9%)        14 (18.7%)   
        61 - 90                              19 (25.3%)        15 (22.4%)         18 (24%)    
        >= 91                                26 (34.7%)        28 (41.8%)        28 (37.3%)   
      Total dose administered                                                                 
        n                                        75                67                75       
        Mean (SD)                          6675.2 (1110.9)   6505.1 (1249.3)   6982.4 (1272.5)
        Median                                 6720.0            6480.0            7200.0     
        Min - Max                          4800.0 - 9360.0   4080.0 - 9360.0   4320.0 - 9360.0
      Total number of doses administered                                                      
        n                                        75                67                75       
        Mean (SD)                             7.0 (0.0)         7.0 (0.0)         7.0 (0.0)   
        Median                                   7.0               7.0               7.0      
        Min - Max                             7.0 - 7.0         7.0 - 7.0         7.0 - 7.0   

# EXT01 variant: with user specified categories for missed doses

    Code
      res
    Output
                                              A: Drug X        B: Placebo      C: Combination 
                                               (N=134)           (N=134)           (N=132)    
      ————————————————————————————————————————————————————————————————————————————————————————
      Treatment duration (days)                                                               
        n                                        75                67                75       
        Mean (SD)                            74.3 (41.6)       79.0 (43.1)       74.2 (39.5)  
        Median                                  77.0              80.0              78.0      
        Min - Max                            5.0 - 149.0       2.0 - 150.0       1.0 - 147.0  
      Treatment duration (days)                                                               
        n                                        75                67                75       
        0 - 30                                12 (16%)         12 (17.9%)         15 (20%)    
        31 - 60                               18 (24%)         12 (17.9%)        14 (18.7%)   
        61 - 90                              19 (25.3%)        15 (22.4%)         18 (24%)    
        >= 91                                26 (34.7%)        28 (41.8%)        28 (37.3%)   
      Total dose administered                                                                 
        n                                        75                67                75       
        Mean (SD)                          6675.2 (1110.9)   6505.1 (1249.3)   6982.4 (1272.5)
        Median                                 6720.0            6480.0            7200.0     
        Min - Max                          4800.0 - 9360.0   4080.0 - 9360.0   4320.0 - 9360.0
      Total number of doses administered                                                      
        n                                        75                67                75       
        Mean (SD)                             7.0 (0.0)         7.0 (0.0)         7.0 (0.0)   
        Median                                   7.0               7.0               7.0      
        Min - Max                             7.0 - 7.0         7.0 - 7.0         7.0 - 7.0   
      Missed Doses                                                                            
        n                                        75                67                75       
        At least 1 missed dose               74 (55.2%)         63 (47%)         73 (55.3%)   
        At least 5 missed doses               59 (44%)         49 (36.6%)        59 (44.7%)   
        At least 10 missed doses             41 (30.6%)        38 (28.4%)         37 (28%)    
        At least 15 missed doses             26 (19.4%)        21 (15.7%)        16 (12.1%)   

