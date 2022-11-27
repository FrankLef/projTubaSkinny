gt_noncompliant <- function(data, row_var = "group", titles = list()) {
  data |> 
    gt(rowname_col = row_var) |>
    gt::tab_spanner_delim(delim = "_", split = "first") |>
    tab_header(
      title = html(titles$title),
      subtitle = html(titles$subtitle)) |>
    fmt_number(columns = where(is.numeric),
               decimals = 0) |>
    grand_summary_rows(
      columns = everything(),
      fns = list(
        Total = ~sum(., na.rm = TRUE)),
      formatter = fmt_number,
      decimals = 0) |>
    tab_source_note("Source: Billing sytem") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = list(cells_body(columns = "sales"),
                       cells_grand_summary(columns = "sales"))) |>
    tab_footnote(
      footnote = "Any observation can have one or more non-compliance.",
      locations = cells_title()) |>
    tab_footnote(
      footnote = "Observations with of zero or less.",
      locations = cells_column_labels(columns = ends_with("zero"))) |>
    tab_footnote(
      footnote = "Out-of-bound values: Very low or very high values.",
      locations = cells_column_labels(columns = ends_with("oob"))) |>
    tab_footnote(
      footnote = "Outlier values, using the mahalanobis distance.",
      locations = cells_column_labels(columns = ends_with("maha"))) |>
    gt::tab_options(
      heading.background.color = "slateblue",
      column_labels.font.weight = "bold",
      column_labels.background.color = "slateblue1",
      row_group.font.weight = "bold",
      row_group.background.color = "slateblue2") |>
    identity()
}
