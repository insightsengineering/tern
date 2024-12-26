# s_count_patients_and_multiple_events works as expected

    Code
      res
    Output
      $unique
      [1] 4
      attr(,"label")
      [1] "counts"
      
      $all
      [1] 7
      attr(,"label")
      [1] "counts"
      
      $serious
      count 
          7 
      attr(,"label")
      [1] "counts"
      
      $fatal
      count 
          4 
      attr(,"label")
      [1] "counts"
      

# s_count_patients_and_multiple_events can have empty stats if requested

    Code
      res
    Output
      $unique
      [1] 4
      attr(,"label")
      [1] "counts"
      
      $all
      character(0)
      attr(,"label")
      [1] "counts"
      
      $serious
      character(0)
      attr(,"label")
      [1] "counts"
      
      $fatal
      count 
          4 
      attr(,"label")
      [1] "counts"
      

# summarize_patients_events_in_cols works well with default arguments

    Code
      res
    Output
               Patients (All)   Events (All)   Events (Related)   fatal   fatal_related
      —————————————————————————————————————————————————————————————————————————————————
      counts         4               7                5             4           4      

# summarize_patients_events_in_cols works well with custom arguments

    Code
      res
    Output
            Related   All
      ———————————————————
      bla      5         

