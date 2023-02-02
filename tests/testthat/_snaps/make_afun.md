# make_afun works with healthy input statistics function taking `df`

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1    nrows              4          0     nrows
      2    ncols           3.00          2     ncols

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod      row_label
      1    ncols              3          1 number columns

# make_afun processes additional rtables arguments correctly

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1    nrows              4          0     nrows
      2    ncols           3.00          2     ncols
      3    incol          FALSE          0     incol
      4   nincol              3          0    nincol

# make_afun works with healthy input function taking `x`

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1        n              8          0         n
      2     mean           2.45          2      mean
      3   median           2.25          1    median

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod         row_label
      1        n              8          0 Number of numbers
      2   median           2.25          3            median

# make_afun produces empty cells and keeps labels when applied to empty character

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod          row_label
      1        n              8          0 Number of patients
      2     mean           2.45          2               Mean
      3   median           2.25          1             Median

---

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod          row_label
      1        n                         0 Number of patients
      2     mean                         2               Mean
      3   median                         1             Median

---

    Code
      res
    Output
                           setosa   versicolor   virginica
      ————————————————————————————————————————————————————
      Number of patients                50          50    

# make_afun by default removes results from `.in_ref_col`

    Code
      res
    Output
                        setosa   versicolor   virginica
      —————————————————————————————————————————————————
      Label for Range              4.9, 7     4.9, 7.9 

# make_afun works with nested lists

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod       row_label
      1  nrow_df            150          0       row count
      2   .N_col             40          0 count in column
      3        a              1          0               a
      4        b              3          0               b

# make_afun can subset on non-nested results when unnesting took place

    Code
      res
    Output
      RowsVerticalSection (in_rows) object print method:
      ----------------------------
        row_name formatted_cell indent_mod row_label
      1  nrow_df         150.00          0   nrow_df

