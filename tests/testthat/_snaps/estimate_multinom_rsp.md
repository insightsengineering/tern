# d_onco_rsp_label provide right response labels

    Code
      res
    Output
                          CR                     NE                     PR 
      Complete Response (CR)     Not Evaluable (NE)  Partial Response (PR) 
      Levels: Complete Response (CR) Partial Response (PR) Not Evaluable (NE)

# d_onco_rsp_label describe label with x being a factor

    Code
      res
    Output
                            CR                       SD                       PR 
        Complete Response (CR)      Stable Disease (SD)    Partial Response (PR) 
                            PD                       NE 
      Progressive Disease (PD)       Not Evaluable (NE) 
      5 Levels: Complete Response (CR) Partial Response (PR) ... Not Evaluable (NE)

# estimate_multinomial_response returns right result

    Code
      res
    Output
                                                A                B                 C       
      —————————————————————————————————————————————————————————————————————————————————————
      Complete Response (CR)                0 (0.0%)         2 (50.0%)        4 (100.0%)   
        95% CI (Wald, with correction)    (0.00, 12.50)    (0.00, 100.00)   (87.50, 100.00)
      Partial Response (PR)                4 (100.0%)        2 (50.0%)         0 (0.0%)    
        95% CI (Wald, with correction)   (87.50, 100.00)   (0.00, 100.00)    (0.00, 12.50) 

