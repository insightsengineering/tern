# f_conf_level works for proportion

    Code
      res
    Output
      [1] "95% CI"

# f_pval works for numeric input

    Code
      res
    Output
      [1] "p-value (H0: mean = 0.05)"

# make_names works as expected

    Code
      res
    Output
      [1] "AnyGrade"        "TotalAEnumbers"  "Noadverseevents"

# get_covariates works for a character vector

    Code
      res
    Output
      $a
      [1] "a"
      
      $b
      [1] "b"
      
      $c
      [1] "c"
      

# month2day works correctly

    Code
      res
    Output
      [1] 403.29688 248.06563  30.43750  86.25988        NA

# day2month works correctly

    Code
      res
    Output
      [1] 13.2402464  8.1478439  0.9856263  2.8254620         NA

# empty_vector_if_na works correctly

    Code
      res
    Output
      numeric(0)

# extract_by_name works for non-NULL input

    Code
      res
    Output
      $b
      function(x) paste(x, "bla")
      <environment: 0x0000012a6922c800>
      

# extract_by_name returns NULL when there is no overlap

    Code
      res
    Output
      NULL

# aesi_label works as expected for SMQ

    Code
      res
    Output
      [1] "AESI 1 (NARROW)"

# aesi_label works as expected for CQ

    Code
      res
    Output
      [1] "AESI CQ1"

# aesi_label works as expected when input includes multiple values

    Code
      res
    Output
      NULL

---

    Code
      res
    Output
      [1] "CQ: ABC"

# n_available works as expected

    Code
      res
    Output
      [1] 3

# range_noinf for INT [no zero-len data, no NAs, no Inf] and with all default options

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, no NAs, no Inf] with
                    [na.rm = TRUE, finite = FALSE (d)]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, no NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, with NAs, no Inf] and with default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [no zero-len data, with NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1 5

# range_noinf for INT [with zero-len data, no NAs, no Inf] and with all default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, no NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, with NAs, no Inf] and with default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for INT [with zero-len data, with NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] and with all default options

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, no NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] and with all default options

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, no NAs, with Inf] with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] and with default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [no zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] 1.5 3.5

# range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] and with all default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, no NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, no NAs, no Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, no NAs, with Inf]
                    and with all default options

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [with zero-len data, no NAs, with Inf]
                    with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [with zero-len data, no NAs, with Inf]
                    with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, no NAs, with Inf]
                    with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, no Inf]
                    and with default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, no Inf] with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] and with default options

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = FALSE (def)]

    Code
      res
    Output
      [1] -Inf  Inf

# range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = FALSE (def), finite = TRUE]

    Code
      res
    Output
      [1] NA NA

# range_noinf for DOUBLE [with zero-len data, with NAs, with Inf] with [na.rm = TRUE, finite = TRUE]

    Code
      res
    Output
      [1] NA NA

