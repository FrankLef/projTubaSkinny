gt_groups_amt <- function(data, row_var = "group",
                          titles = list(title = "Title", subtitle = "Subtitle")) {
  gt(data, rowname_col = row_var) |>
    grand_summary_rows(
      columns = c("sales", "mat", "hrs", "addval", "labor", "profit"),
      fns = list(
        Total = ~sum(., na.rm = TRUE)),
      formatter = fmt_number,
      decimals = 0,
      scale_by = 0.001)|>
    fmt_number(columns = where(is.numeric),
               decimals = 0, scale_by = 0.001) |>
    tab_header(
      title = html(titles$title),
      subtitle = html(titles$subtitle)) |>
    tab_source_note("Source: Transactional sytem") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = cells_grand_summary()) |>
    tab_footnote(
      footnote = "Net Sales.",
      locations = cells_column_labels(columns = "sales")) |>
    tab_footnote(
      footnote = "Cost of materials.",
      locations = cells_column_labels(columns = "mat")) |>
    tab_footnote(
      footnote = "Hours worked.",
      locations = cells_column_labels(columns = "hrs")) |>
    tab_footnote(
      footnote = "Added value. Sales \U2212 materials.",
      locations = cells_column_labels(columns = "addval")) |>
    tab_footnote(
      footnote = "Labor costs. Hours worked x wage.",
      locations = cells_column_labels(columns = "labor")) |>
    tab_footnote(
      footnote = "Gross profit. Sales \U2212 material \U2212 labor.",
      locations = cells_column_labels(columns = "profit")) |>
    tab_footnote(
      footnote = "Total capitalisation. Total LT debt and equity.",
      locations = cells_column_labels(columns = "cap")) |>
    opt_row_striping() |>
    gt::tab_options(
      heading.background.color = "skyblue3",
      column_labels.font.weight = "bold",
      column_labels.background.color = "skyblue2",
      grand_summary_row.background.color = "skyblue1")
}
