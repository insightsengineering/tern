# Patient Disposition table is produced correctly

    Code
      res
    Output
                                            A: Drug X      B: Placebo     C: Combination   All Patients 
                                             (N=134)         (N=134)         (N=132)          (N=400)   
      ——————————————————————————————————————————————————————————————————————————————————————————————————
      Started Study                       134 (100.00%)   134 (100.00%)   132 (100.00%)    400 (100.00%)
      Completed Study                      68 (50.75%)     66 (49.25%)     73 (55.30%)     207 (51.75%) 
      Discontinued Study                   42 (31.34%)     40 (29.85%)     38 (28.79%)     120 (30.00%) 
          ADVERSE EVENT                     3 (2.24%)       6 (4.48%)       5 (3.79%)       14 (3.50%)  
          DEATH                            25 (18.66%)     23 (17.16%)     22 (16.67%)      70 (17.50%) 
          LACK OF EFFICACY                  2 (1.49%)       2 (1.49%)       3 (2.27%)        7 (1.75%)  
          PHYSICIAN DECISION                2 (1.49%)       3 (2.24%)       2 (1.52%)        7 (1.75%)  
          PROTOCOL VIOLATION                5 (3.73%)       3 (2.24%)       4 (3.03%)       12 (3.00%)  
          WITHDRAWAL BY PARENT/GUARDIAN     4 (2.99%)       2 (1.49%)       1 (0.76%)        7 (1.75%)  
          WITHDRAWAL BY SUBJECT             1 (0.75%)       1 (0.75%)       1 (0.76%)        3 (0.75%)  

# Demographic table is produced correctly

    Code
      res
    Output
                                                     A: Drug X    B: Placebo    C: Combination   All Patients
                                                      (N=134)       (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————————————————————————————————————————————
      Age (yr)                                                                                               
        n                                               134           134            132             400     
        Mean (SD)                                   33.8 (6.6)    35.4 (7.9)      35.4 (7.7)      34.9 (7.4) 
        Median                                         33.0          35.0            35.0            34.0    
        Min - Max                                   21.0 - 50.0   21.0 - 62.0    20.0 - 69.0     20.0 - 69.0 
      Age group (yr)                                                                                         
        n                                               134           134            132             400     
        < 65 yrs                                    134 (100%)    134 (100%)     131 (99.2%)     399 (99.8%) 
        >= 65 yrs                                        0             0           1 (0.8%)        1 (0.2%)  
      Sex                                                                                                    
        n                                               134           134            132             400     
        F                                            79 (59%)     82 (61.2%)       70 (53%)      231 (57.8%) 
        M                                            55 (41%)     52 (38.8%)       62 (47%)      169 (42.2%) 
      Race                                                                                                   
        n                                               134           134            132             400     
        ASIAN                                       68 (50.7%)     67 (50%)       73 (55.3%)      208 (52%)  
        BLACK OR AFRICAN AMERICAN                   31 (23.1%)    28 (20.9%)      32 (24.2%)      91 (22.8%) 
        WHITE                                       27 (20.1%)    26 (19.4%)      21 (15.9%)      74 (18.5%) 
        AMERICAN INDIAN OR ALASKA NATIVE              8 (6%)       11 (8.2%)       6 (4.5%)       25 (6.2%)  
        MULTIPLE                                         0         1 (0.7%)           0            1 (0.2%)  
        NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER        0         1 (0.7%)           0            1 (0.2%)  
      Ethnicity                                                                                              
        n                                               134           134            132             400     
        Ethnicity 1                                 50 (37.3%)    51 (38.1%)      46 (34.8%)     147 (36.8%) 
        Ethnicity 2                                 46 (34.3%)    38 (28.4%)      41 (31.1%)     125 (31.2%) 
        Unknown                                     38 (28.4%)    45 (33.6%)      45 (34.1%)      128 (32%)  

# Enrollment by Country Table is produced correctly

    Code
      res
    Output
             A: Drug X    B: Placebo    C: Combination   All Patients
              (N=134)       (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————
      n         134           134            132             400     
      CHN   74 (55.22%)   81 (60.45%)    64 (48.48%)     219 (54.75%)
      USA   10 (7.46%)    13 (9.70%)     17 (12.88%)     40 (10.00%) 
      BRA   13 (9.70%)     7 (5.22%)      10 (7.58%)      30 (7.50%) 
      PAK   12 (8.96%)     9 (6.72%)      10 (7.58%)      31 (7.75%) 
      NGA    8 (5.97%)     7 (5.22%)      11 (8.33%)      26 (6.50%) 
      RUS    5 (3.73%)     8 (5.97%)      6 (4.55%)       19 (4.75%) 
      JPN    5 (3.73%)     4 (2.99%)      9 (6.82%)       18 (4.50%) 
      GBR    4 (2.99%)     3 (2.24%)      2 (1.52%)       9 (2.25%)  
      CAN    3 (2.24%)     2 (1.49%)      3 (2.27%)       8 (2.00%)  
      CHE    0 (0.00%)     0 (0.00%)      0 (0.00%)       0 (0.00%)  

# Death table is produced correctly

    Code
      res
    Output
                                A: Drug X    B: Placebo    C: Combination   All Patients
                                 (N=134)       (N=134)        (N=132)         (N=400)   
      ——————————————————————————————————————————————————————————————————————————————————
      Total Number of Deaths   76 (62.30%)   70 (56.91%)    75 (62.50%)     221 (60.55%)

# Table of Serious Adverse Events is produced correctly (for one specific treatment arm)

    Code
      res
    Output
                                                                         Patients (All)   Events (All)   Events (Related)   Events (Fatal)   Events (Fatal & Related)
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one serious adverse event        104                                                                                    
      cl A.1                                                                                                                                                         
        dcd A.1.1.1.2                                                          48              68               0                 0                     0            
      cl B.1                                                                                                                                                         
        dcd B.1.1.1.1                                                          47              56               56                56                    56           
      cl B.2                                                                                                                                                         
        dcd B.2.2.3.1                                                          48              64               0                 0                     0            
      cl D.1                                                                                                                                                         
        dcd D.1.1.1.1                                                          50              61               61                61                    61           

# Table of Non-Serious Adverse Events is produced correctly

    Code
      res
    Output
                                                                                          A: Drug X                      B: Placebo                    C: Combination        
                                                                                Patients (All)   Events (All)   Patients (All)   Events (All)   Patients (All)   Events (All)
                                                                                   (N=134)         (N=134)         (N=134)         (N=134)         (N=132)         (N=132)   
      ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total number of patients with at least one non-SAE and number of events        106             360             112             367             112             421     
      cl A.1                                                                                                                                                                 
        dcd A.1.1.1.1                                                                 50              64              45              62              63              88     
      cl B.2                                                                                                                                                                 
        dcd B.2.1.2.1                                                                 49              65              44              62              52              66     
      cl C.1                                                                                                                                                                 
        dcd C.1.1.1.3                                                                 43              55              46              63              43              64     
      cl C.2                                                                                                                                                                 
        dcd C.2.1.2.1                                                                 35              48              48              53              55              65     
      cl D.1                                                                                                                                                                 
        dcd D.1.1.4.2                                                                 48              66              42              55              50              64     
      cl D.2                                                                                                                                                                 
        dcd D.2.1.5.3                                                                 47              62              58              72              57              74     

