gt_groups_intrvl <- function(data, rowgroup_var = "rec_id", row_var = "term",
                             titles = list(title = "Title", subtitle = "Subtitle")) {
  gt(data, groupname_col = rowgroup_var, rowname_col = row_var) |>
    fmt_number(columns = c("est_inv", "low_inv", "upr_inv"),
               decimals = 0) |>
    tab_header(
      title = html(titles$title),
      subtitle = html(titles$subtitle)) |>
    tab_source_note("Source: Billing sytem") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = cells_body(columns = "est_inv")
    ) |>
    cols_label(
      est_inv = "Average",
      low_inv = "Low 67%",
      upr_inv = "High 67%") |>
    tab_footnote(
      footnote = "Variable portion only. Fixed portion must be added to obtain the total sales.",
      locations = cells_title(groups = "subtitle")) |>
    tab_footnote(
      footnote = "Average price.",
      locations = cells_column_labels(columns = "est_inv")) |>
    tab_footnote(
      footnote = "Lower price at 67% of all values.",
      locations = cells_column_labels(columns = "low_inv")) |>
    tab_footnote(
      footnote = "Upper price at 67% of all values.",
      locations = cells_column_labels(columns = "upr_inv")) |>
    tab_footnote(
      footnote = "Dollars per hour worked.",
      locations = cells_row_groups(groups = "hrs")) |>
    tab_footnote(
      footnote = "Dollars per dollars of material.",
      locations = cells_row_groups(groups = "mat")) |>
    gt::tab_options(
      heading.background.color = "aquamarine",
      column_labels.font.weight = "bold",
      column_labels.background.color = "aquamarine1",
      row_group.font.weight = "bold",
      row_group.background.color = "aquamarine2")
}
