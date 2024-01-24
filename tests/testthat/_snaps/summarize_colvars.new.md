# summarize_colvars works as expected without column split and default behavior

    Code
      res
    Output
                      AVAL          CHG    
      —————————————————————————————————————
      V1                                   
        n               3            3     
        Mean (SD)   6.0 (3.0)    0.0 (0.0) 
        Median         6.0          0.0    
        Min - Max   3.0 - 9.0    0.0 - 0.0 
      V2                                   
        n               3            3     
        Mean (SD)   5.0 (3.0)   -1.0 (0.0) 
        Median         5.0         -1.0    
        Min - Max   2.0 - 8.0   -1.0 - -1.0
      V3                                   
        n               3            3     
        Mean (SD)   4.0 (3.0)   -2.0 (0.0) 
        Median         4.0         -2.0    
        Min - Max   1.0 - 7.0   -2.0 - -2.0

# summarize_colvars works as expected with column split

    Code
      res
    Output
                                     A                                      B                  
                          AVAL                CHG                AVAL                CHG       
      —————————————————————————————————————————————————————————————————————————————————————————
      V1                                                                                       
        n                  2                   2                  1                   1        
        Mean (SD)      6.0 (4.2)           0.0 (0.0)       6.0 (<no-value>)   0.0 (<no-value>) 
        Median            6.0                 0.0                6.0                 0.0       
        Min - Max      3.0 - 9.0           0.0 - 0.0          6.0 - 6.0           0.0 - 0.0    
      V2                                                                                       
        n                  1                   1                  2                   2        
        Mean (SD)   5.0 (<no-value>)   -1.0 (<no-value>)      5.0 (4.2)          -1.0 (0.0)    
        Median            5.0                -1.0                5.0                -1.0       
        Min - Max      5.0 - 5.0          -1.0 - -1.0         2.0 - 8.0          -1.0 - -1.0   
      V3                                                                                       
        n                  2                   2                  1                   1        
        Mean (SD)      4.0 (4.2)          -2.0 (0.0)       4.0 (<no-value>)   -2.0 (<no-value>)
        Median            4.0                -2.0                4.0                -2.0       
        Min - Max      1.0 - 7.0          -2.0 - -2.0         4.0 - 4.0          -2.0 - -2.0   

# summarize_colvars works when selecting statistics and custom formatting

    Code
      res
    Output
                                             A                                    B                 
                                  AVAL               CHG               AVAL               CHG       
      ——————————————————————————————————————————————————————————————————————————————————————————————
      V1                                                                                            
            n                       2                 2                  1                 1        
                  Mean, SD      6.0, 4.2           0.0, 0.0       6.0, <no-value>   0.0, <no-value> 
      V2                                                                                            
            n                       1                 1                  2                 2        
                  Mean, SD   5.0, <no-value>   -1.0, <no-value>      5.0, 4.2          -1.0, 0.0    
      V3                                                                                            
            n                       2                 2                  1                 1        
                  Mean, SD      4.0, 4.2          -2.0, 0.0       4.0, <no-value>   -2.0, <no-value>

