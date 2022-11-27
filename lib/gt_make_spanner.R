gt_make_spanner <- function(data, row_var, col_var, spann_var, val_var, 
                            delim = "_", split = c("first", "last")) {
  
  split = match.arg(split)
  
  unite_var <- paste(col_var, spann_var, sep = delim)
  data |>
    dplyr::select(all_of(c(row_var, col_var, spann_var, val_var))) |>
    tidyr::unite(col = unite_var, all_of(c(col_var, spann_var)), sep = delim) |>
    tidyr::pivot_wider(id_cols = all_of(row_var), names_from = unite_var, 
                       values_from = all_of(val_var)) |>
    gt::gt(rowname_col = row_var) |>
    gt::tab_spanner_delim(delim = delim, split = split)
}
