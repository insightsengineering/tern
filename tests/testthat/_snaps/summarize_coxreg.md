# s_coxreg converts tabulated results in a list

    Code
      res
    Output
      $hr
      $hr$`2 vs control (1)`
      [1] 0.6386426
      
      
      $hr
      $hr$`A Covariate Label`
      [1] 0.607037
      
      
      $hr
      $hr$`Sex (F/M)`
      [1] 0.6242738
      
      

# s_coxreg works with which_vars and var_nms arguments

    Code
      res
    Output
      $hr
      $hr$`  F`
      [1] 0.6678243
      
      $hr$`  M`
      [1] 0.5954021
      
      

# s_coxreg works with character covariates in the univariate case when interaction = TRUE

    Code
      res
    Output
      $hr
      $hr$`2 vs control (1)`
      [1] 0.6386426
      
      
      $hr
      $hr$`A Covariate Label`
      numeric(0)
      
      $hr$`  1`
      [1] 0.6284569
      
      $hr$`  2`
      [1] 0.5806499
      
      $hr$`  3`
      [1] 0.5486103
      
      $hr$`  4`
      [1] 0.6910725
      
      
      $hr
      $hr$`Sex (F/M)`
      numeric(0)
      
      $hr$`  F`
      [1] 0.6678243
      
      $hr$`  M`
      [1] 0.5954021
      
      

# a_coxreg works as expected

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1  Label 1            340          0   Label 1

# summarize_coxreg adds the univariate Cox regression layer to rtables

    Code
      res
    Output
                             n    Hazard Ratio      90% CI      p-value
      —————————————————————————————————————————————————————————————————
      Treatment:                                                       
        2 vs control (1)    340       0.64       (0.46, 0.89)   0.0253 
      Covariate:                                                       
        A Covariate Label   340       0.61       (0.44, 0.85)   0.0136 
        Sex (F/M)           340       0.63       (0.45, 0.87)   0.0191 

---

    Code
      res
    Output
                            n    Hazard Ratio      95% CI      p-value
      ————————————————————————————————————————————————————————————————
      Treatment:                                                      
        2 vs control (1)   340       0.64       (0.43, 0.94)   0.0242 
      Covariate:                                                      
        First Covariate    340       0.61       (0.41, 0.90)   0.0126 
        Second Covariate   340       0.62       (0.42, 0.92)   0.0182 

---

    Code
      res
    Output
                            n    Hazard Ratio      90% CI      p-value
      ————————————————————————————————————————————————————————————————
      Treatment:                                                      
        2 vs control (1)   340       0.64       (0.46, 0.89)   0.0253 
      Covariate:                                                      
        COVAR1             340       0.61       (0.44, 0.85)   0.0136 
        COVAR2             340       0.63       (0.45, 0.87)   0.0191 

# summarize_coxreg .section_div argument works

    Code
      res
    Output
                             n    Hazard Ratio      95% CI      p-value
      —————————————————————————————————————————————————————————————————
      Treatment:                                                       
        2 vs control (1)    340       0.64       (0.43, 0.94)   0.0242 
      _________________________________________________________________
      Covariate:                                                       
        A Covariate Label   340       0.61       (0.41, 0.90)   0.0126 
       * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
        Sex (F/M)           340       0.62       (0.42, 0.92)   0.0182 

# summarize_coxreg works with interactions in univariate case

    Code
      res
    Output
                             n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ———————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                             
        2 vs control (1)    340       0.64       (0.43, 0.94)   0.0242                       
      Covariate:                                                                             
        Age                 340                                                 0.1757       
          40                          0.62       (0.42, 0.93)                                
        A Covariate Label   340                                                 0.9883       
          1                           0.63       (0.35, 1.14)                                
          2                           0.58       (0.27, 1.26)                                
          3                           0.55       (0.22, 1.35)                                
          4                           0.69       (0.23, 2.07)                                
        Sex (F/M)           340                                                 0.7759       
          F                           0.67       (0.36, 1.22)                                
          M                           0.60       (0.36, 0.99)                                

# summarize_coxreg 'at' argument works in univariate case

    Code
      res
    Output
                            n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ——————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                            
        2 vs control (1)   340       0.64       (0.43, 0.94)   0.0242                       
      Covariate:                                                                            
        Age                340                                                 0.1757       
          15                         0.35       (0.13, 0.92)                                
          30                         0.49       (0.29, 0.86)                                
          60                         0.98       (0.47, 2.04)                                
        Sex (F/M)          340                                                 0.7759       
          F                          0.67       (0.36, 1.22)                                
          M                          0.60       (0.36, 0.99)                                

# summarize_coxreg `na_str` argument works

    Code
      res
    Output
                             n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ———————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                             
        2 vs control (1)    340       0.64       (0.43, 0.94)   0.0242            ---        
      Covariate:                                                                             
        A Covariate Label   340                                                 0.9883       
          1                           0.63       (0.35, 1.14)                     ---        
          2                           0.58       (0.27, 1.26)                     ---        
          3                           0.55       (0.22, 1.35)                     ---        
          4                           0.69       (0.23, 2.07)                     ---        
        Sex (F/M)           340                                                 0.7759       
          F                           0.67       (0.36, 1.22)                     ---        
          M                           0.60       (0.36, 0.99)                     ---        

# summarize_coxreg works without treatment arm in univariate case

    Code
      res
    Output
                                            Hazard Ratio      90% CI      p-value
      ———————————————————————————————————————————————————————————————————————————
      Covariate:                                                                 
        A Covariate Label (reference = 1)                                 <0.0001
          2                                     0.45       (0.30, 0.66)   0.0007 
          3                                     0.31       (0.20, 0.48)   <0.0001
          4                                     0.18       (0.11, 0.30)   <0.0001
        Sex (F/M) (reference = F)                                                
          M                                     1.33       (0.97, 1.82)   0.1414 

# summarize_coxreg works with numeric covariate without treatment arm in univariate case

    Code
      res
    Output
                                            Hazard Ratio      90% CI      p-value
      ———————————————————————————————————————————————————————————————————————————
      Covariate:                                                                 
        A Covariate Label (reference = 1)                                 <0.0001
          2                                     0.45       (0.30, 0.66)   0.0007 
          3                                     0.31       (0.20, 0.48)   <0.0001
          4                                     0.18       (0.11, 0.30)   <0.0001
        Age                                                                      
          All                                   1.01       (1.00, 1.02)   0.2486 

# summarize_coxreg works with character covariate in univariate case when interaction = TRUE

    Code
      res
    Output
                                      n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                      
        ARM: 2 vs control (ARM: 1)   340       0.64       (0.43, 0.94)   0.0242                       
      Covariate:                                                                                      
        COVAR2                       340                                                 0.7759       
          F                                    0.67       (0.36, 1.22)                                
          M                                    0.60       (0.36, 0.99)                                

---

    Code
      res
    Output
                                      n    Hazard Ratio      95% CI      p-value   Interaction p-value
      ————————————————————————————————————————————————————————————————————————————————————————————————
      Treatment:                                                                                      
        ARM: 2 vs control (ARM: 1)   340       0.64       (0.43, 0.94)   0.0242                       
      Covariate:                                                                                      
        A Covariate Label            340                                                 0.9883       
          1                                    0.63       (0.35, 1.14)                                
          2                                    0.58       (0.27, 1.26)                                
          3                                    0.55       (0.22, 1.35)                                
          4                                    0.69       (0.23, 2.07)                                
        COVAR2                       340                                                 0.7759       
          F                                    0.67       (0.36, 1.22)                                
          M                                    0.60       (0.36, 0.99)                                

# summarize_coxreg adds the multivariate Cox regression layer to rtables

    Code
      res
    Output
                                            Hazard Ratio      95% CI      p-value
      ———————————————————————————————————————————————————————————————————————————
      Treatment:                                                                 
        ARM (reference = 1)                                                      
          2                                     0.60       (0.40, 0.88)   0.0100 
      Covariate:                                                                 
        Age                                                                      
          All                                   1.01       (0.99, 1.03)   0.2097 
        A Covariate Label (reference = 1)                                 <0.0001
          2                                     0.47       (0.29, 0.75)   0.0017 
          3                                     0.31       (0.18, 0.51)   <0.0001
          4                                     0.18       (0.10, 0.33)   <0.0001
        Sex (F/M) (reference = F)                                                
          M                                     1.31       (0.89, 1.92)   0.1665 

---

    Code
      res
    Output
                                           Hazard Ratio      95% CI      p-value
      ——————————————————————————————————————————————————————————————————————————
      Treatment:                                                                
        ARM (reference = 1)                                                     
          2                                    0.60       (0.40, 0.88)   0.0100 
      Covariate:                                                                
        Age Covariate                                                           
          All                                  1.01       (0.99, 1.03)   0.2097 
        First Covariate (reference = 1)                                  <0.0001
          2                                    0.47       (0.29, 0.75)   0.0017 
          3                                    0.31       (0.18, 0.51)   <0.0001
          4                                    0.18       (0.10, 0.33)   <0.0001
        Second Covariate (reference = F)                                        
          M                                    1.31       (0.89, 1.92)   0.1665 

---

    Code
      res
    Output
                                 Hazard Ratio      95% CI      p-value
      ————————————————————————————————————————————————————————————————
      Treatment:                                                      
        ARMCD (reference = 1)                                         
          2                          0.60       (0.40, 0.88)   0.0100 
      Covariate:                                                      
        AGE                                                           
          All                        1.01       (0.99, 1.03)   0.2097 
        COVAR1 (reference = 1)                                 <0.0001
          2                          0.47       (0.29, 0.75)   0.0017 
          3                          0.31       (0.18, 0.51)   <0.0001
          4                          0.18       (0.10, 0.33)   <0.0001
        COVAR2 (reference = F)                                        
          M                          1.31       (0.89, 1.92)   0.1665 

