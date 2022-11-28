# setup -------------------------------------------------------------------
stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)
# tmp$data


# matrix summaries --------------------------------------------------------

tmp$corr_data <- tmp$data |>
  select(sales, mat, hrs, sales_lga, mat_lga, hrs_lga)

tmp$corr_summ <- tmp$corr_data |> 
  summ_corr(corr_digits = 3, stats_digits = 3)
tmp$corr_summ

tmp$gt_corr <- tmp$corr_summ |> 
  gt(rowname_col = "term") |>
  # format corr
  fmt_number(
    columns = !matches("term"),
    rows = !(term %in% c("mean", "sd")),
    decimals = 2) |>
  # format stats
  fmt_number(
    columns = !matches("term"),
    rows = term %in% c("mean", "sd"),
    decimals = 2) |>
  sub_missing(missing_text = "-") |>
  data_color(
    columns = !matches("term"),
    colors = scales::col_numeric(
      palette = c("red", "orange", "green", "blue"),
      domain = range(-1, 1),
      na.color = "white"
    )) |>
  tab_style(
    style = cell_fill(color = "moccasin"),
    locations = cells_body(
      columns = !matches("term"),
      rows = term %in% c("mean", "sd")
    )) |>
  identity()
tmp$gt_corr

tmp$corr <- tmp$data |>
  select(sales_lga, mat_lga, hrs_lga) |>
  # select(sales, mat, hrs) |>
  as.data.frame() |>
  cor()
# tmp$corr

tmp$ggp_corr <- tmp$corr|>
  ggcorrplot::ggcorrplot(hc.order = TRUE,outline.color = "grey", type = "lower",
                         lab = TRUE, lab_col = "violetred", lab_size = 12,
                         digits = 2,
                         show.legend = FALSE, show.diag = FALSE,
                         title = "Correlations") +
  scale_fill_continuous(type = "viridis")
# tmp$ggp_corr


# matrix summaries by group -----------------------------------------------

tmp$stats_grp <- tmp$data |>
  select(group, sales_lga, mat_lga, hrs_lga) |>
  group_by(group)|>
  summarize(across(.cols = everything(), .fns = list(mean = mean, sd = sd))) |>
  identity()
# tmp$stats_grp



# teardown ----------------------------------------------------------------


# xprts_obj <- set_bag(xprts_obj, name = "ggp_groups_roi_addval", 
#                      value = tmp$ggp_groups_roi_addval)

suppressWarnings(rm(tmp))

