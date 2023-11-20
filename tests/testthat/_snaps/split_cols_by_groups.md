# groups_list_to_df works as expected

    Code
      res
    Output
         valname         label    levelcombo exargs
      1 AnyGrade Any Grade (%) 1, 2, 3, 4, 5   NULL
      2  Grade34 Grade 3-4 (%)          3, 4   NULL
      3   Grade5   Grade 5 (%)             5   NULL

# combine_groups combines character vectors

    Code
      res
    Output
      $A
      [1] "A"
      
      $`B/C`
      [1] "B" "C"
      

# combine_groups combines factors

    Code
      res
    Output
      $A
      [1] "A"
      
      $`B/C`
      [1] "B" "C"
      

# combine_groups combines factors with given reference

    Code
      res
    Output
      $B
      [1] "B"
      
      $`A/C`
      [1] "A" "C"
      

# combine_groups use good separator

    Code
      res
    Output
      $A
      [1] "A"
      
      $`B||C`
      [1] "B" "C"
      

# combine_groups can use multiple reference

    Code
      res
    Output
      $`B&C`
      [1] "B" "C"
      
      $A
      [1] "A"
      

# split_cols_by_groups manages combinations of columns

    Code
      res
    Output
             Arms A+B   Arms A+C
             (N=227)    (N=250) 
      ——————————————————————————
      Mean    34.03      34.73  

# split_cols_by_groups manages combinations of columns with reference

    Code
      res
    Output
                          Arms A+B   Arms A+C
      ———————————————————————————————————————
      Diff. of Averages                0.71  

# split_cols_by_groups equivalent to split_cols_by when no groups

    Code
      res
    Output
             A: Drug X   B: Placebo   C: Combination
              (N=121)     (N=106)        (N=129)    
      ——————————————————————————————————————————————
      Mean     34.91       33.02          34.57     

# split_cols_by_groups equivalent to split_cols_by with ref_col but no groups

    Code
      res
    Output
                          B: Placebo   A: Drug X   C: Combination
                           (N=106)      (N=121)       (N=129)    
      ———————————————————————————————————————————————————————————
      Diff. of Averages                  1.89           1.55     

# split_cols_by_groups manages combinations of columns with reference and alt_counts_df

    Code
      res
    Output
                          Arms A+B   Arms A+C
                          (N=227)    (N=250) 
      ———————————————————————————————————————
      Diff. of Averages               -0.75  

# combine_counts combines character vectors

    Code
      res
    Output
        A B/C 
        3   3 

# combine_counts combines factors

    Code
      res
    Output
        A B/C 
        3   3 

---

    Code
      res
    Output
      A/C   B 
        4   2 

# combine_counts with groups_list NULL

    Code
      res
    Output
      A B C 
      3 2 1 

