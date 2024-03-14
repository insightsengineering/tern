# h_data_plot works as expected

    Code
      res
    Output
      [1] "`g_km` now generates `ggplot` objects"

# h_data_plot respects the ordering of the arm variable factor levels

    Code
      res
    Output
      [1] "ARM B" "ARM C" "ARM A"

# h_data_plot adds rows that have time 0 and estimate 1

    Code
      res
    Output
      [1] ARM A ARM B ARM C
      Levels: ARM A ARM B ARM C

