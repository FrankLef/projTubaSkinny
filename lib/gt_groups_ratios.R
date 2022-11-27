gt_groups_ratios <- function(obj,
                             titles = list(title = "Title", subtitle = "Subtitle")) {
  checkmate::assertClass(obj, classes = "gt_tbl")
  obj |>
    tab_header(title = html(titles$title), subtitle = html(titles$subtitle)) |>
    cols_label(
      med.sales2mat = "Average",
      low_hinge.sales2mat = "Low 25%",
      high_hinge.sales2mat = "High 75%",
      med.addval2hrs = "Average",
      low_hinge.addval2hrs = "Low 25%",
      high_hinge.addval2hrs = "High 75%") |>
    fmt_number(columns = !matches("group"), decimals = 2) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = cells_body(columns = matches("med"))) |>
    tab_style(
      style = list(
        cell_borders(sides = "left")),
      locations = list(
        cells_body(
          rows = everything(),
          columns = "low_hinge.addval2hrs"),
        cells_column_labels(columns = "low_hinge.addval2hrs"))) |>
    tab_source_note("Source: Billing sytem") |>
    tab_footnote(
      footnote = "Average price.",
      locations = cells_column_labels(columns = matches("med"))) |>
    tab_footnote(
      footnote = "Lower price at 25% of all values.",
      locations = cells_column_labels(columns = matches("low_hinge"))) |>
    tab_footnote(
      footnote = "Upper price at 75% of all values.",
      locations = cells_column_labels(columns = matches("high_hinge"))) |>
    tab_footnote(
      footnote = "Added value per hour worked.",
      locations = cells_column_spanners(spanners = matches("addval2hrs"))) |>
    tab_footnote(
      footnote = "Sales per dollar of material.",
      locations = cells_column_spanners(spanners = matches("sales2mat"))) |>
    gt::tab_options(
      heading.background.color = "wheat",
      column_labels.font.weight = "bold",
      column_labels.background.color = "wheat1",
      row_group.font.weight = "bold",
      row_group.background.color = "wheat2")
}
