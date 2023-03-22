# h_adlb_worsen stacks data correctly (simple case)

    Code
      res
    Output
      # A tibble: 20 x 8
      # Groups:   USUBJID, PARAMCD [15]
         USUBJID PARAMCD   VALUES     MIN   MAX WGRLOFL WGRHIFL GRADDR
         <chr>   <chr>      <dbl>   <dbl> <dbl> <chr>   <chr>   <chr> 
       1 4       ABC       0.0478  0.0478  187. "Y"     ""      Low   
       2 3       OPQ       0.314   0.314   144. "Y"     ""      Low   
       3 4       OPQ       0.455   0.455   193. "Y"     ""      Low   
       4 2       ABC       0.790   0.790   181. "Y"     ""      Low   
       5 2       OPQ       7.49    7.49    195. "Y"     ""      Low   
       6 5       OPQ      22.7    22.7     187. "Y"     ""      Low   
       7 1       OPQ      23.5    23.5     196. "Y"     ""      Low   
       8 1       ABC      26.9    26.9     187. "Y"     ""      Low   
       9 3       ABC      37.9    37.9     197. "Y"     ""      Low   
      10 5       ABC      46.7    46.7     193. "Y"     ""      Low   
      11 3       OPQ     144.      0.314   144. ""      "Y"     High  
      12 3       XYZ     155.     17.1     155. ""      "Y"     High  
      13 2       XYZ     157.      7.79    157. ""      "Y"     High  
      14 4       XYZ     167.      0.276   167. ""      "Y"     High  
      15 5       OPQ     187.     22.7     187. ""      "Y"     High  
      16 5       XYZ     189.     16.1     189. ""      "Y"     High  
      17 4       OPQ     193.      0.455   193. ""      "Y"     High  
      18 2       OPQ     195.      7.49    195. ""      "Y"     High  
      19 1       OPQ     196.     23.5     196. ""      "Y"     High  
      20 1       XYZ     198.     16.5     198. ""      "Y"     High  

# h_adlb_worsen stacks data correctly

    Code
      res
    Output
      # A tibble: 8 x 9
        USUBJID              ARMCD AVISIT PARAMCD ATOXGR BTOXGR WGRLOFL WGRHIFL GRADDR
        <chr>                <fct> <fct>  <chr>   <fct>  <fct>  <chr>   <chr>   <chr> 
      1 AB12345-CHN-1-id-53  ARM B WEEK ~ IGA     0      0      ""      "Y"     High  
      2 AB12345-CHN-3-id-128 ARM B WEEK ~ IGA     4      0      ""      "Y"     High  
      3 AB12345-CHN-1-id-53  ARM B WEEK ~ ALT     4      0      ""      "Y"     High  
      4 AB12345-CHN-3-id-128 ARM B WEEK ~ ALT     1      0      ""      "Y"     High  
      5 AB12345-CHN-1-id-53  ARM B WEEK ~ CRP     0      1      "Y"     "Y"     Low   
      6 AB12345-CHN-3-id-128 ARM B WEEK ~ CRP     0      0      "Y"     ""      Low   
      7 AB12345-CHN-1-id-53  ARM B WEEK ~ ALT     -4     0      "Y"     ""      Low   
      8 AB12345-CHN-3-id-128 ARM B WEEK ~ ALT     -4     0      "Y"     ""      Low   

# h_worsen_counter counts data (low) correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# h_worsen_counter counts data (high) correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# h_worsen_counter counts data (low), no high correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          2    10 
      
      $fraction$`2`
        num denom 
          3    15 
      
      $fraction$`3`
        num denom 
          4    20 
      
      $fraction$`4`
        num denom 
          5    25 
      
      $fraction$Any
        num denom 
         14    25 
      
      

# h_worsen_counter counts data (low), no low correctly

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          0    30 
      
      $fraction$`2`
        num denom 
          0    30 
      
      $fraction$`3`
        num denom 
          0    30 
      
      $fraction$`4`
        num denom 
          0    30 
      
      $fraction$Any
        num denom 
          0    30 
      
      

# s_count_abnormal_lab_worsen_by_baseline

    Code
      res
    Output
      $fraction
      $fraction$`1`
        num denom 
          6    54 
      
      $fraction$`2`
        num denom 
          7    63 
      
      $fraction$`3`
        num denom 
          8    72 
      
      $fraction$`4`
        num denom 
          9    81 
      
      $fraction$Any
        num denom 
         30    81 
      
      

# count_abnormal_lab_worsen_by_baseline

    Code
      res
    Output
                    ARM A           ARM B           ARM C    
                   (N=69)          (N=73)          (N=58)    
      ———————————————————————————————————————————————————————
      IGA                                                    
        High                                                 
          1      6/63 (9.5%)     6/64 (9.4%)      4/50 (8%)  
          2     8/64 (12.5%)     5/67 (7.5%)    8/53 (15.1%) 
          3     7/66 (10.6%)     5/68 (7.4%)    9/57 (15.8%) 
          4      6/68 (8.8%)     2/72 (2.8%)     3/58 (5.2%) 
          Any   27/68 (39.7%)    18/72 (25%)    24/58 (41.4%)
      ALT                                                    
        High                                                 
          1     7/63 (11.1%)     6/62 (9.7%)     2/48 (4.2%) 
          2      12/63 (19%)      4/67 (6%)      11/50 (22%) 
          3      4/65 (6.2%)    11/71 (15.5%)   7/56 (12.5%) 
          4      1/67 (1.5%)    8/71 (11.3%)      4/57 (7%)  
          Any   24/67 (35.8%)   29/71 (40.8%)   24/57 (42.1%)
        Low                                                  
          1     12/67 (17.9%)    4/66 (6.1%)    7/52 (13.5%) 
          2     9/68 (13.2%)    12/69 (17.4%)   6/55 (10.9%) 
          3      6/69 (8.7%)     4/71 (5.6%)     5/56 (8.9%) 
          4     7/69 (10.1%)     7/73 (9.6%)    6/58 (10.3%) 
          Any   34/69 (49.3%)    27/73 (37%)    24/58 (41.4%)
      CRP                                                    
        Low                                                  
          1     11/66 (16.7%)   10/67 (14.9%)    4/47 (8.5%) 
          2     8/66 (12.1%)     1/70 (1.4%)     6/50 (12%)  
          3      4/68 (5.9%)    9/70 (12.9%)     5/53 (9.4%) 
          4     7/69 (10.1%)     6/72 (8.3%)     4/55 (7.3%) 
          Any   30/69 (43.5%)   26/72 (36.1%)   19/55 (34.5%)

# h_adlb_worsen all high

    Code
      res
    Output
      # A tibble: 600 x 51
         STUDYID COUNTRY SITEID SUBJID   AGE SEX   ARMCD ARM      ACTAR~1 ACTARM RACE 
         <chr>   <fct>   <chr>  <chr>  <dbl> <fct> <fct> <fct>    <fct>   <fct>  <fct>
       1 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       2 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       3 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       4 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       5 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       6 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       7 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       8 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       9 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
      10 AB12345 BRA     BRA-1  id-23   41.3 F     ARM A A: Drug~ ARM A   A: Dr~ AMER~
      # ... with 590 more rows, 40 more variables: TRTSDTM <dttm>, TRTEDTM <dttm>,
      #   EOSDY <dbl>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>, BMRKR2 <fct>,
      #   REGION1 <fct>, SAFFL <fct>, USUBJID <chr>, PARAM <fct>, AVISIT <fct>,
      #   AVAL <dbl>, LBCAT <fct>, PARAMCD <chr>, AVALU <fct>, AVISITN <dbl>,
      #   ABLFL2 <chr>, ABLFL <chr>, BASE <dbl>, BASETYPE <chr>, ANRIND <fct>,
      #   ANRLO <dbl>, ANRHI <dbl>, DTYPE <lgl>, ATOXGR <fct>, BTOXGR <fct>,
      #   ATOXDSCL <chr>, ATOXDSCH <chr>, ADTM <dttm>, ASPID <int>, LBSEQ <int>, ...

# h_adlb_worsen all low

    Code
      res
    Output
      # A tibble: 600 x 51
         STUDYID COUNTRY SITEID SUBJID   AGE SEX   ARMCD ARM      ACTAR~1 ACTARM RACE 
         <chr>   <fct>   <chr>  <chr>  <dbl> <fct> <fct> <fct>    <fct>   <fct>  <fct>
       1 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       2 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       3 AB12345 BRA     BRA-1  id-105  37.8 F     ARM A A: Drug~ ARM A   A: Dr~ ASIAN
       4 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       5 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       6 AB12345 BRA     BRA-1  id-171  29.8 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       7 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       8 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
       9 AB12345 BRA     BRA-1  id-177  38.9 F     ARM B B: Plac~ ARM B   B: Pl~ ASIAN
      10 AB12345 BRA     BRA-1  id-23   41.3 F     ARM A A: Drug~ ARM A   A: Dr~ AMER~
      # ... with 590 more rows, 40 more variables: TRTSDTM <dttm>, TRTEDTM <dttm>,
      #   EOSDY <dbl>, STRATA1 <fct>, STRATA2 <fct>, BMRKR1 <dbl>, BMRKR2 <fct>,
      #   REGION1 <fct>, SAFFL <fct>, USUBJID <chr>, PARAM <fct>, AVISIT <fct>,
      #   AVAL <dbl>, LBCAT <fct>, PARAMCD <chr>, AVALU <fct>, AVISITN <dbl>,
      #   ABLFL2 <chr>, ABLFL <chr>, BASE <dbl>, BASETYPE <chr>, ANRIND <fct>,
      #   ANRLO <dbl>, ANRHI <dbl>, DTYPE <lgl>, ATOXGR <fct>, BTOXGR <fct>,
      #   ATOXDSCL <chr>, ATOXDSCH <chr>, ADTM <dttm>, ASPID <int>, LBSEQ <int>, ...

