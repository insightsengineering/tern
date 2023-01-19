# AOVT01 variant with single endpoint is produced correctly

    Code
      res
    Output
                                                           ARM A        ARM B           ARM C    
                                                          (N=134)      (N=134)         (N=132)   
      ———————————————————————————————————————————————————————————————————————————————————————————
      Unadjusted comparison                                                                      
        n                                                   68           73              62      
        Mean                                               3.68         5.07            3.09     
        Difference in Means                                             1.38            -0.59    
          95% CI                                                    (-2.76, 5.53)   (-4.91, 3.73)
          p-value                                                      0.5113          0.7873    
      Adjusted comparison (covariates BASE and STRATA1)                                          
        n                                                   68           73              62      
        Adjusted Mean                                      4.06         3.57            3.34     
        Difference in Adjusted Means                                    -0.49           -0.72    
          95% CI                                                    (-3.28, 2.29)   (-3.57, 2.12)
          p-value                                                      0.7277          0.6165    

# AOVT01 variant with multiple endpoints is produced correctly

    Code
      res
    Output
                                          ARM A        ARM B           ARM C    
                                         (N=134)      (N=134)         (N=132)   
      ——————————————————————————————————————————————————————————————————————————
      BFIALL                                                                    
        Adjusted mean                                                           
          n                                134          134             132     
          Adjusted Mean                   4.47         6.33            4.02     
          Difference in Adjusted Means                 1.85            -0.46    
            95% CI                                 (-0.14, 3.85)   (-2.45, 1.54)
            p-value                                   0.0679          0.6539    
      FATIGI                                                                    
        Adjusted mean                                                           
          n                                134          134             132     
          Adjusted Mean                   5.42         4.83            4.56     
          Difference in Adjusted Means                 -0.59           -0.86    
            95% CI                                 (-2.58, 1.41)   (-2.87, 1.15)
            p-value                                   0.5644          0.4026    
      FKSI-FWB                                                                  
        Adjusted mean                                                           
          n                                134          134             132     
          Adjusted Mean                   4.29         3.51            3.06     
          Difference in Adjusted Means                 -0.79           -1.24    
            95% CI                                 (-2.71, 1.14)   (-3.17, 0.69)
            p-value                                   0.4221          0.2088    
      FKSI-TSE                                                                  
        Adjusted mean                                                           
          n                                134          134             132     
          Adjusted Mean                   4.70         3.84            4.45     
          Difference in Adjusted Means                 -0.86           -0.25    
            95% CI                                 (-2.80, 1.09)   (-2.20, 1.70)
            p-value                                   0.3858          0.8007    
      FKSIALL                                                                   
        Adjusted mean                                                           
          n                                134          134             132     
          Adjusted Mean                   5.03         5.82            6.44     
          Difference in Adjusted Means                 0.79            1.42     
            95% CI                                 (-1.17, 2.76)   (-0.56, 3.39)
            p-value                                   0.4288          0.1591    

