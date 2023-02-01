# as_factor_keep_attributes works correctly for a character vector

    Code
      res
    Output
      [1] a b
      attr(,"label")
      [1] alphabet
      Levels: a b

# as_factor_keep_attributes converts empty strings for a character vector

    Code
      res
    Output
      [1] a       missing b      
      attr(,"label")
      [1] alphabet
      Levels: a b missing

# bins_percent_labels works as expected

    Code
      res
    Output
      [1] "[0%,20%]"   "(20%,56%]"  "(56%,80%]"  "(80%,100%]"

---

    Code
      res
    Output
      [1] "[0%,20%]"    "(20%,55.5%]" "(55.5%,80%]" "(80%,100%]" 

---

    Code
      res
    Output
      [1] "[0%,100%]"

# cut_quantile_bins works as expected with default settings

    Code
      res
    Output
      [1] "[0%,25%]"   "(25%,50%]"  "(50%,75%]"  "(75%,100%]"

# cut_quantile_bins works with custom quantiles

    Code
      res
    Output
      [1] "[0%,10%]"   "(10%,30%]"  "(30%,80%]"  "(80%,100%]"

# cut_quantile_bins works with custom labels

    Code
      res
    Output
      [1] "low"    "medium" "high"   "top"   

# cut_quantile_bins also works when there are only NAs

    Code
      res
    Output
      [1] "[0%,25%]"   "(25%,50%]"  "(50%,75%]"  "(75%,100%]"

# fct_discard works as expected

    Code
      res
    Output
      [1] a c
      Levels: a c

# fct_explicit_na_if works as expected with factor input

    Code
      res
    Output
      [1] <Missing> b         <Missing>
      Levels: a b <Missing>

# fct_collapse_only works as expected

    Code
      res
    Output
      [1] <Missing> TRT       CTRL      CTRL     
      Levels: TRT CTRL <Missing>

# fct_collapse_only uses the customized `na_level` as expected

    Code
      res
    Output
      [1] <NA>    TRT     Missing CTRL   
      Levels: TRT CTRL Missing

# fct_collapse_only works as expected with character input

    Code
      res
    Output
      [1] <Missing> TRT       CTRL      CTRL     
      Levels: TRT CTRL <Missing>

