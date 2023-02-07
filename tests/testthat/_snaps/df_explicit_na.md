# Default fill in of missing values and conversion to factor works as expected

    Code
      res
    Output
        v1        v2        v3 v4        v5        v6 v7    v8
      1  A         A         A  D         A         A  1  TRUE
      2  B         B         B  E         B         B  2 FALSE
      3  A         A         A  F <Missing> <Missing>  3    NA
      4  B <Missing> <Missing>  E         C         C  4    NA

# Default settings work when input data does not have labels

    Code
      res
    Output
        v1        v2        v3 v4        v5        v6 v7    v8
      1  A         A         A  D         A         A  1  TRUE
      2  B         B         B  E         B         B  2 FALSE
      3  A         A         A  F <Missing> <Missing>  3    NA
      4  B <Missing> <Missing>  E         C         C  4    NA

# Only replace missing values without modifying character or logical variables

    Code
      res
    Output
        v1        v2        v3 v4        v5        v6 v7    v8
      1  A         A         A  D         A         A  1  TRUE
      2  B         B         B  E         B         B  2 FALSE
      3  A         A         A  F <Missing> <Missing>  3    NA
      4  B <Missing> <Missing>  E         C         C  4    NA

# Conversion to factor works with some variables omitted

    Code
      res
    Output
        v1   v2        v3 v4        v5 v6 v7        v8
      1  A    A         A  D         A  A  1      TRUE
      2  B    B         B  E         B  B  2     FALSE
      3  A    A         A  F <Missing>     3 <Missing>
      4  B <NA> <Missing>  E         C  C  4 <Missing>

# Only convert logical variables but not character variables

    Code
      res
    Output
        v1        v2        v3 v4        v5        v6 v7        v8
      1  A         A         A  D         A         A  1      TRUE
      2  B         B         B  E         B         B  2     FALSE
      3  A         A         A  F <Missing> <Missing>  3 <Missing>
      4  B <Missing> <Missing>  E         C         C  4 <Missing>

