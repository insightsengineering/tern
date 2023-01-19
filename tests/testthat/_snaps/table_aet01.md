# Safety Summary Variant 1 works as expected

    Code
      res
    Output
                                                                    A: Drug X    B: Placebo    C: Combination
                                                                     (N=134)       (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event     122 (91.0%)   123 (91.8%)    120 (90.9%)  
      Total AEs                                                        609           622            703      
      Total number of deaths                                       25 (18.66%)   23 (17.16%)    22 (16.67%)  
      Total number of patients withdrawn from study due to an AE    3 (2.24%)     6 (4.48%)      5 (3.79%)   
      Total number of patients with at least one                                                             
        AE with fatal outcome                                      76 (56.7%)    70 (52.2%)      75 (56.8%)  
        Serious AE                                                 104 (77.6%)   101 (75.4%)     99 (75.0%)  
        Serious AE leading to withdrawal from treatment             9 (6.7%)      6 (4.5%)       11 (8.3%)   
        Serious AE leading to dose modification/interruption       22 (16.4%)    26 (19.4%)      29 (22.0%)  
        Related Serious AE                                         76 (56.7%)    70 (52.2%)      75 (56.8%)  
        AE leading to withdrawal from treatment                    27 (20.1%)    26 (19.4%)      30 (22.7%)  
        AE leading to dose modification/interruption               66 (49.3%)    76 (56.7%)      74 (56.1%)  
        Related AE                                                 105 (78.4%)   108 (80.6%)    109 (82.6%)  
        Related AE leading to withdrawal from treatment             6 (4.5%)      12 (9.0%)       8 (6.1%)   
        Related AE leading to dose modification/interruption       29 (21.6%)    38 (28.4%)      38 (28.8%)  
        Grade 3-5 AE                                               109 (81.3%)   104 (77.6%)    109 (82.6%)  

# Safety Summary Variant 2 (with Medical Concepts Section) works as expected

    Code
      res
    Output
                                                                    A: Drug X    B: Placebo    C: Combination
                                                                     (N=134)       (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event     122 (91.0%)   123 (91.8%)    120 (90.9%)  
      Total AEs                                                        609           622            703      
      Total number of deaths                                       25 (18.66%)   23 (17.16%)    22 (16.67%)  
      Total number of patients withdrawn from study due to an AE    3 (2.24%)     6 (4.48%)      5 (3.79%)   
      Total number of patients with at least one                                                             
        AE with fatal outcome                                      76 (56.7%)    70 (52.2%)      75 (56.8%)  
        Serious AE                                                 104 (77.6%)   101 (75.4%)     99 (75.0%)  
        Serious AE leading to withdrawal from treatment             9 (6.7%)      6 (4.5%)       11 (8.3%)   
        Serious AE leading to dose modification/interruption       22 (16.4%)    26 (19.4%)      29 (22.0%)  
        Related Serious AE                                         76 (56.7%)    70 (52.2%)      75 (56.8%)  
        AE leading to withdrawal from treatment                    27 (20.1%)    26 (19.4%)      30 (22.7%)  
        AE leading to dose modification/interruption               66 (49.3%)    76 (56.7%)      74 (56.1%)  
        Related AE                                                 105 (78.4%)   108 (80.6%)    109 (82.6%)  
        Related AE leading to withdrawal from treatment             6 (4.5%)      12 (9.0%)       8 (6.1%)   
        Related AE leading to dose modification/interruption       29 (21.6%)    38 (28.4%)      38 (28.8%)  
        Grade 3-5 AE                                               109 (81.3%)   104 (77.6%)    109 (82.6%)  
      Total number of patients with at least one                                                             
        C.1.1.1.3/B.2.2.3.1 AESI (BROAD)                           72 (53.7%)    79 (59.0%)      75 (56.8%)  
        SMQ 02 Reference Name                                           0             0              0       
        D.2.1.5.3/A.1.1.1.1 AESI                                   74 (55.2%)    80 (59.7%)      87 (65.9%)  

# Safety Summary Variant 3 (with Modified Rows) works as expected

    Code
      res
    Output
                                                                    A: Drug X    B: Placebo    C: Combination
                                                                     (N=134)       (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event     122 (91.0%)   123 (91.8%)    120 (90.9%)  
      Total AEs                                                        609           622            703      
      Total number of deaths                                       25 (18.66%)   23 (17.16%)    22 (16.67%)  
      Total number of patients withdrawn from study due to an AE    3 (2.24%)     6 (4.48%)      5 (3.79%)   
      Total number of patients withdrawn informed consent           1 (0.75%)     1 (0.75%)      1 (0.76%)   
      Total number of patients with at least one                                                             
        AE with fatal outcome                                      76 (56.7%)    70 (52.2%)      75 (56.8%)  
        Serious AE                                                 104 (77.6%)   101 (75.4%)     99 (75.0%)  
        AE leading to withdrawal from treatment                    27 (20.1%)    26 (19.4%)      30 (22.7%)  
        Related AE                                                 105 (78.4%)   108 (80.6%)    109 (82.6%)  
        Grade 3-5 AE                                               109 (81.3%)   104 (77.6%)    109 (82.6%)  
        Grade 4/5 AE                                               91 (67.9%)    90 (67.2%)      93 (70.5%)  

# Safety Summary Variant 4 (with Rows Counting Events and Additional Sections) works as expected

    Code
      res
    Output
                                                                    A: Drug X    B: Placebo    C: Combination
                                                                     (N=134)       (N=134)        (N=132)    
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one adverse event     122 (91.0%)   123 (91.8%)    120 (90.9%)  
      Total AEs                                                        609           622            703      
      Total number of deaths                                       25 (18.66%)   23 (17.16%)    22 (16.67%)  
      Total number of patients withdrawn from study due to an AE    3 (2.24%)     6 (4.48%)      5 (3.79%)   
      Total number of patients with at least one                                                             
        AE with fatal outcome                                      76 (56.7%)    70 (52.2%)      75 (56.8%)  
        Serious AE                                                 104 (77.6%)   101 (75.4%)     99 (75.0%)  
        AE leading to withdrawal from treatment                    27 (20.1%)    26 (19.4%)      30 (22.7%)  
        AE leading to dose modification/interruption               66 (49.3%)    76 (56.7%)      74 (56.1%)  
        Related AE                                                 105 (78.4%)   108 (80.6%)    109 (82.6%)  
        Grade 3-5 AE                                               109 (81.3%)   104 (77.6%)    109 (82.6%)  
      Total number of unique preferred terms which are                                                       
        Serious AE                                                      4             4              4       
        AE leading to dose modification/interruption                    8             8              8       
        Related AE                                                      5             5              5       
        Grade 3-5 AE                                                    5             5              5       
        Grade 4/5                                                       3             3              3       
      Total number of adverse events which are                                                               
        Serious AE                                                     249           255            282      
        AE leading to dose modification/interruption                   116           122            137      
        Related AE                                                     282           299            336      
        Grade 3-5 AE                                                   303           291            327      
        Grade 4/5                                                      172           174            197      

