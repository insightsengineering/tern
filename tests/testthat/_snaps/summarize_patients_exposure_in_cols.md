# s_count_patients_sum_exposure works as expected

    Code
      res
    Output
      $n_patients
      [1] 12  1
      attr(,"label")
      [1] "Total patients numbers/person time"
      
      $sum_exposure
      [1] 35
      attr(,"label")
      [1] "Total patients numbers/person time"
      

# summarize_patients_exposure_in_cols works well with default arguments

    Code
      res
    Output
                                                    ARM A                      ARM B                       Total          
                                            Patients    Person time    Patients    Person time    Patients     Person time
      ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
      Total patients numbers/person time   6 (100.0%)       16        6 (100.0%)       19        12 (100.0%)       35     
        Female                             6 (100.0%)       16         0 (0.0%)         0         6 (50.0%)        16     
        Male                                0 (0.0%)         0        6 (100.0%)       19         6 (50.0%)        19     

# summarize_patients_exposure_in_cols works well with custom arguments

    Code
      res
    Output
                    ARM A         ARM B         Total   
                 Person time   Person time   Person time
      ——————————————————————————————————————————————————
      xyz            16            19            35     
        Female       16             0            16     
        Male          0            19            19     

# summarize_patients_exposure_in_cols returns correct column label when no variable split and only one statistic

    Code
      res
    Output
      [1] "Patients"

