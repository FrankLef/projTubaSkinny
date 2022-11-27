gt_groups_roi <- function(data, row_var = "group",
                          titles = list(title = "Title", subtitle = "Subtitle")) {
  gt(data, rowname_col = row_var) |>
    tab_spanner(
      label = "Added Value ROI",
      columns = c("addval2cap", "addval2hrs", "hrs2cap")) |>
    tab_spanner(
      label = "Profit ROI",
      columns = c("profit2cap", "profit2sales", "sales2cap")) |>
    cols_label(
      addval2cap = "Value ROI",
      addval2hrs = "Price / hr",
      hrs2cap = "Volume",
      profit2cap = "Profit ROI",
      profit2sales = "Profit Margin",
      sales2cap = "Volume") |>
    fmt_percent(columns = c("addval2cap", "profit2cap", "profit2sales"),
                decimals = 1) |>
    fmt_number(columns = c("addval2hrs"),
               decimals = 2) |>
    fmt_number(columns = c("hrs2cap", "sales2cap"),
               decimals = 4) |>
    tab_header(
      title = html(titles$title),
      subtitle = html(titles$subtitle)) |>
    tab_source_note("Source: Transactional sytem") |>
    tab_style(
      style = cell_borders(sides = "left", color = "grey", 
                           style = "solid", weight = px(1)),
      locations = list(
        cells_body(columns = "profit2cap",
                   rows = everything()),
        cells_column_labels(columns = "profit2cap"))) |>
    tab_style(
      style = list(cell_fill(color = "burlywood1"),
                   cell_text(weight = "bold")),
      locations = list(
        cells_body(columns = everything(),
                   rows = group == "Total"),
        cells_stub(rows = "Total"))) |>
    tab_footnote(
      footnote = "ROI = Price \U00D7 Volume = Added value \U00F7 Capital",
      locations = cells_column_labels(columns = "addval2cap")) |>
    tab_footnote(
      footnote = "Added value \U00F7 Hours worked",
      locations = cells_column_labels(columns = "addval2hrs")) |>
    tab_footnote(
      footnote = "Volume ratio = Hours worked \U00F7 Capital.",
      locations = cells_column_labels(columns = "hrs2cap")) |>
    tab_footnote(
      footnote = "ROI = Price \U00D7 Volume = Profit Margin \U00F7 Capital",
      locations = cells_column_labels(columns = "profit2cap")) |>
    tab_footnote(
      footnote = "Profit \U00F7 Sales",
      locations = cells_column_labels(columns = "profit2sales")) |>
    tab_footnote(
      footnote = "Volume ratio = Sales \U00F7 Capital.",
      locations = cells_column_labels(columns = "sales2cap")) |>
    opt_row_striping() |>
    gt::tab_options(
      heading.background.color = "burlywood2",
      column_labels.font.weight = "bold",
      column_labels.background.color = "burlywood1")
}
