gt_corr <- function(data, row_var, group_var = NULL,
                    terms = c("mean", "sd"),
                    colrs = list(pal = "oompaBase::jetColors"),
                    titles = list()) {
  colrs$pal <- unclass(paletteer::paletteer_c(colrs$pal, n = 16))
  
  data |> 
    gt(rowname_col = row_var, groupname_col = group_var) |>
    tab_header(
      title = html(paste0("<b>", titles$title, "</b>")),
      subtitle = html(paste0("<b>", titles$subtitle, "</b>"))) |>
    # format corr numbers
    fmt_number(
      columns = !all_of(c(row_var, group_var)),
      rows = !(term %in% terms),
      decimals = 2) |>
    # format stats numbers
    fmt_number(
      columns = !all_of(c(row_var, group_var)),
      rows = term %in% terms,
      decimals = 2) |>
    sub_missing(missing_text = "-") |>
    data_color(
      columns = !all_of(c(row_var, group_var)),
      colors = scales::col_numeric(
        palette = colrs$pal,
        domain = c(-1, 1),
        na.color = "transparent")) |>
    suppressWarnings() |>
    # format title
    tab_style(
      style = list(cell_text(color = "darkblue")),
      locations = cells_title()) |>
    # format stats rows
    tab_style(
      style = list(
        cell_fill(color = "transparent"),
        cell_text(color ="darkblue", weight ="normal", style = "italic")),
      locations = cells_body(
        columns = !all_of(c(row_var, group_var)),
        rows = term %in% terms)) |>
    identity()
}
