#tabulation function for condition checks
t_helper_tabulate <- function(df_id, N, checkcol, term, remove_dupl, with_percent){
  
  if(remove_dupl){
    df_id <- df_id[!duplicated(df_id$id), ]
  }
  if(checkcol == " "){
    if(with_percent){
      tbl <- rtabulate(
        na.omit(df_id),
        row_by_var = no_by(""),
        col_by_var = "col_by",
        FUN = count_perc_col_N,
        N = N,
        format = "xx (xx.xx%)"
      )
    }
    else{
      tbl <- rtabulate(
        na.omit(df_id),
        row_by_var = no_by(""),
        col_by_var = "col_by",
        FUN = count_col_N,
        N = N,
        format = "xx"
      )
    }
  }
  else{
    tbl <- rtabulate(
      na.omit(df_id),
      row_by_var = checkcol,
      col_by_var = "col_by",
      FUN = count_perc_col_N,
      N = N,
      format = "xx (xx.xx%)"
    )
    
    if(dim(tbl)[1] > 1){
      tbl <- tbl[dim(tbl)[1]]
    }
    else{
      for(i in 1:dim(tbl)[2]){
        tbl[[1]][[i]] <- rcell(0)
      }
    }
    
  }
  
  attr(tbl[[1]], "row.name") <- term
  
  header(tbl) <- rheader(
    rrowl("", levels(df_id$col_by)),
    rrowl("", unname(N), format = "(N=xx)")
  )
  tbl
  
}

# checks if there is any case and derives counts, otherwise 0
count_col_N <- function(x_cell, N) {
  N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
  if (N_i > 0) {
    length(x_cell$id) # obtaining the total
  } else {
    rcell(0, format = "xx")
  }
}