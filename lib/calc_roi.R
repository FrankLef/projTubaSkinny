calc_roi <- function(data, sales_var = "sales", mat_var = "mat", labor_var = "hrs",
                     cap_var = "cap", addval_var = "addval", profit_var = "profit") {
  data |>
    dplyr::mutate(
      {{addval_var}} := .data[[sales_var]] - .data[[mat_var]],
      {{profit_var}} := .data[[sales_var]] - .data[[addval_var]]) |>
    dplyr::mutate(
      addval2cap = .data[[addval_var]] / .data[[cap_var]],
      addval2hrs = .data[[addval_var]] / .data[[labor_var]],
      hrs2cap = .data[[labor_var]] / .data[[cap_var]],
      sales2addval = .data[[sales_var]] / .data[[addval_var]],
      profit2cap = .data[[profit_var]] / .data[[cap_var]],
      profit2sales = .data[[profit_var]] / .data[[sales_var]],
      sales2cap = data[[sales_var]] / data[[cap_var]],
      sales2mat = .data[[sales_var]] / .data[[mat_var]]) |>
    assertr::verify(dplyr::near(addval2hrs * hrs2cap, addval2cap)) |>
    assertr::verify(dplyr::near(profit2sales * sales2cap, profit2cap))
}
