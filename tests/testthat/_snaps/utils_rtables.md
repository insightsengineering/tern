# to_string_matrix works correctly

    Code
      res
    Output
            [,1]                        [,2]        [,3]         [,4]            
       [1,] ""                          "A: Drug X" "B: Placebo" "C: Combination"
       [2,] "ASIAN"                     ""          ""           ""              
       [3,] "  A"                       ""          ""           ""              
       [4,] "    mean"                  "32.19"     "33.90"      "36.81"         
       [5,] "  B"                       ""          ""           ""              
       [6,] "    mean"                  "34.12"     "31.62"      "34.73"         
       [7,] "  C"                       ""          ""           ""              
       [8,] "    mean"                  "36.21"     "33.00"      "32.39"         
       [9,] "BLACK OR AFRICAN AMERICAN" ""          ""           ""              
      [10,] "  A"                       ""          ""           ""              
      [11,] "    mean"                  "31.50"     "28.57"      "33.62"         
      [12,] "  B"                       ""          ""           ""              
      [13,] "    mean"                  "35.60"     "30.83"      "33.67"         
      [14,] "  C"                       ""          ""           ""              
      [15,] "    mean"                  "35.50"     "34.18"      "35.00"         
      [16,] "WHITE"                     ""          ""           ""              
      [17,] "  A"                       ""          ""           ""              
      [18,] "    mean"                  "37.67"     "31.33"      "33.17"         
      [19,] "  B"                       ""          ""           ""              
      [20,] "    mean"                  "39.86"     "39.00"      "34.75"         
      [21,] "  C"                       ""          ""           ""              
      [22,] "    mean"                  "39.75"     "44.67"      "36.75"         

---

    Code
      res
    Output
       [1] "                            A: Drug X   B: Placebo   C: Combination"
       [2] "———————————————————————————————————————————————————————————————————"
       [3] "ASIAN                                                              "
       [4] "  A                                                                "
       [5] "    mean                      32.19       33.90          36.81     "
       [6] "  B                                                                "
       [7] "    mean                      34.12       31.62          34.73     "
       [8] "  C                                                                "
       [9] "    mean                      36.21       33.00          32.39     "
      [10] "BLACK OR AFRICAN AMERICAN                                          "
      [11] "  A                                                                "
      [12] "    mean                      31.50       28.57          33.62     "
      [13] "  B                                                                "
      [14] "    mean                      35.60       30.83          33.67     "
      [15] "  C                                                                "
      [16] "    mean                      35.50       34.18          35.00     "
      [17] "WHITE                                                              "
      [18] "  A                                                                "
      [19] "    mean                      37.67       31.33          33.17     "
      [20] "  B                                                                "
      [21] "    mean                      39.86       39.00          34.75     "
      [22] "  C                                                                "
      [23] "    mean                      39.75       44.67          36.75     "

---

    Code
      print_result
    Output
       [1] "c("                                                        
       [2] "  \"\", \"A: Drug X\", \"B: Placebo\", \"C: Combination\","
       [3] "  \"ASIAN\", \"\", \"\", \"\","                            
       [4] "  \"  A\", \"\", \"\", \"\","                              
       [5] "  \"    mean\", \"32.19\", \"33.90\", \"36.81\","          
       [6] "  \"  B\", \"\", \"\", \"\","                              
       [7] "  \"    mean\", \"34.12\", \"31.62\", \"34.73\","          
       [8] "  \"  C\", \"\", \"\", \"\","                              
       [9] "  \"    mean\", \"36.21\", \"33.00\", \"32.39\","          
      [10] "  \"BLACK OR AFRICAN AMERICAN\", \"\", \"\", \"\","        
      [11] "  \"  A\", \"\", \"\", \"\","                              
      [12] "  \"    mean\", \"31.50\", \"28.57\", \"33.62\","          
      [13] "  \"  B\", \"\", \"\", \"\","                              
      [14] "  \"    mean\", \"35.60\", \"30.83\", \"33.67\","          
      [15] "  \"  C\", \"\", \"\", \"\","                              
      [16] "  \"    mean\", \"35.50\", \"34.18\", \"35.00\","          
      [17] "  \"WHITE\", \"\", \"\", \"\","                            
      [18] "  \"  A\", \"\", \"\", \"\","                              
      [19] "  \"    mean\", \"37.67\", \"31.33\", \"33.17\","          
      [20] "  \"  B\", \"\", \"\", \"\","                              
      [21] "  \"    mean\", \"39.86\", \"39.00\", \"34.75\","          
      [22] "  \"  C\", \"\", \"\", \"\","                              
      [23] "  \"    mean\", \"39.75\", \"44.67\", \"36.75\""           
      [24] ")"                                                         

---

    Code
      res
    Output
       [1] "c("                                                                        
       [2] "  \"                            A: Drug X   B: Placebo   C: Combination\","
       [3] "  \"———————————————————————————————————————————————————————————————————\","
       [4] "  \"ASIAN                                                              \","
       [5] "  \"  A                                                                \","
       [6] "  \"    mean                      32.19       33.90          36.81     \","
       [7] "  \"  B                                                                \","
       [8] "  \"    mean                      34.12       31.62          34.73     \","
       [9] "  \"  C                                                                \","
      [10] "  \"    mean                      36.21       33.00          32.39     \","
      [11] "  \"BLACK OR AFRICAN AMERICAN                                          \","
      [12] "  \"  A                                                                \","
      [13] "  \"    mean                      31.50       28.57          33.62     \","
      [14] "  \"  B                                                                \","
      [15] "  \"    mean                      35.60       30.83          33.67     \","
      [16] "  \"  C                                                                \","
      [17] "  \"    mean                      35.50       34.18          35.00     \","
      [18] "  \"WHITE                                                              \","
      [19] "  \"  A                                                                \","
      [20] "  \"    mean                      37.67       31.33          33.17     \","
      [21] "  \"  B                                                                \","
      [22] "  \"    mean                      39.86       39.00          34.75     \","
      [23] "  \"  C                                                                \","
      [24] "  \"    mean                      39.75       44.67          36.75     \"" 
      [25] ")"                                                                         

# unlist_and_blank_na works as expected if not all missing

    Code
      res
    Output
      [1]  1  3  5 NA

# unlist_and_blank_na works as expected if all missing

    Code
      res
    Output
      character(0)

# cfun_by_flag works as expected

    Code
      res
    Output
      rcell: 1.0000 

# labels_or_names works correctly

    Code
      res
    Output
          a     b 
        "a" "bla" 

---

    Code
      res
    Output
            b 
       "" "b" 

---

    Code
      res
    Output
                b 
      "bli"   "b" 

---

    Code
      res
    Output
      [1] "" ""

# c_label_n works as expected

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
         row_name formatted_cell indent_mod    row_label
      1 row_count                         0 female (N=4)

# c_label_n_alt works as expected

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
         row_name formatted_cell indent_mod     row_label
      1 row_count                         0 female (N=10)

# add_rowcounts works with one row split

    Code
      res
    Output
                  all obs
      ———————————————————
      F (N=187)          
      M (N=169)          

# add_rowcounts works with multiple column and row splits

    Code
      res
    Output
                          A: Drug X              B: Placebo            C: Combination    
                      A       B       C       A       B       C       A       B       C  
      ———————————————————————————————————————————————————————————————————————————————————
      CHN (N=179)                                                                        
        F (N=94)                                                                         
          mean      30.92   36.91   35.36   34.33   32.89   39.75   35.33   33.40   34.75
        M (N=85)                                                                         
          mean      36.29   38.00   39.46   30.00   32.00   32.80   34.82   33.00   31.87
      USA (N=44)                                                                         
        F (N=24)                                                                         
          mean      33.00   43.00   41.33   27.50    NA     32.25   34.20   37.00   36.00
        M (N=20)                                                                         
          mean      35.00   36.50   35.50   40.00   34.00   28.00   39.25   31.00   39.50
      BRA (N=29)                                                                         
        F (N=15)                                                                         
          mean      29.00   28.75   47.00   31.00   30.50   24.00   37.00   39.00   34.00
        M (N=14)                                                                         
          mean       NA     31.00   33.00   32.33   31.67   35.00    NA     48.00   31.67
      PAK (N=28)                                                                         
        F (N=12)                                                                         
          mean       NA     35.00    NA     46.00   29.67   42.00   44.00   25.50   34.00
        M (N=16)                                                                         
          mean      36.67   37.00   33.00   28.00    NA     32.00   41.00   38.50   36.33
      NGA (N=24)                                                                         
        F (N=13)                                                                         
          mean      26.50   28.50   24.00   31.00    NA      NA     32.00   40.00   30.50
        M (N=11)                                                                         
          mean      32.00   37.00   42.50    NA     21.00   37.00   35.00   44.00    NA  
      RUS (N=20)                                                                         
        F (N=10)                                                                         
          mean      30.00   36.50   32.75    NA      NA     40.00    NA     36.50    NA  
        M (N=10)                                                                         
          mean      27.00    NA     39.00   30.00   36.50   28.00   35.50   27.00   27.00
      JPN (N=18)                                                                         
        F (N=9)                                                                          
          mean       NA     35.00    NA      NA     41.50   35.00   43.00   40.00   32.50
        M (N=9)                                                                          
          mean      33.00    NA     26.50   29.50   27.67    NA     33.00    NA      NA  
      GBR (N=7)                                                                          
        F (N=6)                                                                          
          mean       NA     32.00    NA     29.00    NA      NA     30.00    NA      NA  
        M (N=1)                                                                          
          mean       NA      NA      NA      NA      NA      NA      NA     30.00    NA  
      CAN (N=7)                                                                          
        F (N=4)                                                                          
          mean      32.50   43.00    NA      NA     30.00    NA      NA      NA      NA  
        M (N=3)                                                                          
          mean       NA      NA      NA      NA     38.00    NA     29.50    NA      NA  

# add_rowcounts works with pruning

    Code
      res
    Output
                                    A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————
      F (N=187)                                                            
        ASIAN                          44           37             40      
        BLACK OR AFRICAN AMERICAN      18           12             13      
        WHITE                           8           7              8       
      M (N=169)                                                            
        ASIAN                          35           31             44      
        BLACK OR AFRICAN AMERICAN      10           12             14      
        WHITE                           6           7              10      

---

    Code
      res
    Output
                                    A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————
      F (N=187)                                                            
        ASIAN                          44           37             40      
        BLACK OR AFRICAN AMERICAN      18           12             13      
        WHITE                           8           7              8       

# add_rowcounts works with alt_counts = TRUE

    Code
      res
    Output
                                    A: Drug X   B: Placebo   C: Combination
      —————————————————————————————————————————————————————————————————————
      F (N=52)                                                             
        ASIAN                          44           37             40      
        BLACK OR AFRICAN AMERICAN      18           12             13      
        WHITE                           8           7              8       
      M (N=48)                                                             
        ASIAN                          35           31             44      
        BLACK OR AFRICAN AMERICAN      10           12             14      
        WHITE                           6           7              10      

# h_col_indices works as expected

    Code
      res
    Output
      [1] 2 3

# as.rtable.data.frame works correctly

    Code
      res
    Output
           a      b  
      ———————————————
      A   1.0    10.0
      B   2.0    11.1
      C   3.0    12.2
      D   4.0    13.3
      E   5.0    14.4
      F   6.0    15.6
      G   7.0    16.7
      H   8.0    17.8
      I   9.0    18.9
      J   10.0   20.0

# as.rtable.data.frame uses variable labels for column headers when they are available

    Code
      res
    Output
      [1] "label for a" "label for b"

# h_split_param divides param values

    Code
      res
    Output
      $surv
      [1] "pt_at_risk"
      
      $surv_diff
      [1] "rate_diff"
      

---

    Code
      res
    Output
      $surv
           pt_at_risk event_free_rate 
                 "xx"           "xxx" 
      
      $surv_diff
      NULL
      

# afun_selected_stats works for NULL input

    Code
      res
    Output
      [1] "b"

# afun_selected_stats works for character input

    Code
      res
    Output
      [1] "c"

# append_varlabels works as expected

    Code
      res
    Output
      SEX                   A: Drug X          B: Placebo       C: Combination 
        Age                  (N=121)            (N=106)            (N=129)     
      —————————————————————————————————————————————————————————————————————————
      F                                                                        
        mean             33.7142857142857   33.8392857142857   34.8852459016393
      M                                                                        
        mean             36.5490196078431         32.1         34.2794117647059
      U                                                                        
        mean                    NA                 NA                 NA       
      UNDIFFERENTIATED                                                         
        mean                    NA                 NA                 NA       

# append_varlabels correctly concatenates multiple variable labels

    Code
      res
    Output
      SEX / Age             A: Drug X          B: Placebo       C: Combination 
      —————————————————————————————————————————————————————————————————————————
      F                                                                        
        mean             33.7142857142857   33.8392857142857   34.8852459016393
      M                                                                        
        mean             36.5490196078431         32.1         34.2794117647059
      U                                                                        
        mean                    NA                 NA                 NA       
      UNDIFFERENTIATED                                                         
        mean                    NA                 NA                 NA       

