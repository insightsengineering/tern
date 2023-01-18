# DST01 default variant is produced correctly

    Code
      res
    Output
                                 A: Drug X    B: Placebo    C: Combination   All Patients
                                  (N=134)       (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————————————————————————
      Completed Study           65 (48.51%)   67 (50.00%)    69 (52.27%)     201 (50.25%)
      Discontinued Study        15 (11.2%)    28 (20.9%)      24 (18.2%)      67 (16.8%) 
        Death                     6 (40%)      5 (17.9%)      4 (16.7%)       15 (22.4%) 
        Lost To Follow-Up        1 (6.7%)      3 (10.7%)      8 (33.3%)       12 (17.9%) 
        Other                    2 (13.3%)     6 (21.4%)      5 (20.8%)       13 (19.4%) 
        Protocol Violation       4 (26.7%)     4 (14.3%)      4 (16.7%)       12 (17.9%) 
        Withdrawal By Subject    2 (13.3%)    10 (35.7%)      3 (12.5%)       15 (22.4%) 

# DST01 variant with grouping of reasons is produced correctly

    Code
      res
    Output
                                  A: Drug X      B: Placebo     C: Combination   All Patients 
                                   (N=134)         (N=134)         (N=132)          (N=400)   
      ————————————————————————————————————————————————————————————————————————————————————————
      Received treatment        134 (100.00%)   134 (100.00%)   132 (100.00%)    400 (100.00%)
      Discontinued treatment     70 (52.2%)      65 (48.5%)       68 (51.5%)      203 (50.7%) 
        Safety                   21 (15.7%)      28 (20.9%)       26 (19.7%)      75 (18.8%)  
          ADVERSE EVENT          10 (47.6%)      10 (35.7%)       11 (42.3%)      31 (41.3%)  
          PHYSICIAN DECISION     11 (52.4%)      18 (64.3%)       15 (57.7%)      44 (58.7%)  
        Other                    49 (36.6%)      37 (27.6%)       42 (31.8%)      128 (32.0%) 
          LACK OF EFFICACY       14 (28.6%)      14 (37.8%)       10 (23.8%)      38 (29.7%)  
          OTHER                  20 (40.8%)       10 (27%)        15 (35.7%)      45 (35.2%)  
          PROGRESSIVE DISEASE    15 (30.6%)      13 (35.1%)       17 (40.5%)      45 (35.2%)  

# DST01 variant with adding other optional rows is produced correctly

    Code
      res
    Output
                                 A: Drug X    B: Placebo    C: Combination   All Patients
                                  (N=134)       (N=134)        (N=132)         (N=400)   
      ———————————————————————————————————————————————————————————————————————————————————
      Completed Study           65 (48.51%)   67 (50.00%)    69 (52.27%)     201 (50.25%)
      Alive: In Follow-up       25 (46.30%)   20 (51.28%)    18 (46.15%)     63 (47.73%) 
      Discontinued Study        15 (11.2%)    28 (20.9%)      24 (18.2%)      67 (16.8%) 
        Death                     6 (40%)      5 (17.9%)      4 (16.7%)       15 (22.4%) 
        Lost To Follow-Up        1 (6.7%)      3 (10.7%)      8 (33.3%)       12 (17.9%) 
        Other                    2 (13.3%)     6 (21.4%)      5 (20.8%)       13 (19.4%) 
        Protocol Violation       4 (26.7%)     4 (14.3%)      4 (16.7%)       12 (17.9%) 
        Withdrawal By Subject    2 (13.3%)    10 (35.7%)      3 (12.5%)       15 (22.4%) 

