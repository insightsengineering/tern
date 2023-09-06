# get_stats works as expected for defaults

    Code
      res
    Output
      [1] "count"                   "count_fraction_fixed_dp"
      [3] "fraction"               

---

    Code
      res
    Output
      [1] "unique"       "nonunique"    "unique_count"

---

    Code
      res
    Output
      [1] "n"              "count"          "count_fraction" "n_blq"         

---

    Code
      res
    Output
       [1] "n"            "sum"          "mean"         "sd"           "se"          
       [6] "mean_sd"      "mean_se"      "mean_ci"      "mean_sei"     "mean_sdi"    
      [11] "mean_pval"    "median"       "mad"          "median_ci"    "quantiles"   
      [16] "iqr"          "range"        "min"          "max"          "median_range"
      [21] "cv"           "geom_mean"    "geom_mean_ci" "geom_cv"     

# get_format_from_stats works as expected

    Code
      res
    Output
      $count
      [1] "xx."
      
      $count_fraction_fixed_dp
      function(x, ...) {
        attr(x, "label") <- NULL
      
        if (any(is.na(x))) {
          return("NA")
        }
      
        checkmate::assert_vector(x)
        checkmate::assert_integerish(x[1])
        assert_proportion_value(x[2], include_boundaries = TRUE)
      
        result <- if (x[1] == 0) {
          "0"
        } else if (x[2] == 1) {
          sprintf("%d (100%%)", x[1])
        } else {
          sprintf("%d (%.1f%%)", x[1], x[2] * 100)
        }
      
        return(result)
      }
      <environment: namespace:tern>
      
      $fraction
      function(x, ...) {
        attr(x, "label") <- NULL
        checkmate::assert_vector(x)
        checkmate::assert_count(x["num"])
        checkmate::assert_count(x["denom"])
      
        result <- if (x["num"] == 0) {
          paste0(x["num"], "/", x["denom"])
        } else {
          paste0(
            x["num"], "/", x["denom"],
            " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
          )
        }
        return(result)
      }
      <environment: namespace:tern>
      

# get_label_from_stats works as expected

    Code
      res
    Output
                        count count_fraction_fixed_dp                fraction 
                      "count"                      ""                      "" 

