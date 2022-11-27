# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()


tmp$data <- get_data(sales)
# check nb of rows
tmp$data_n <- nrow(tmp$data)

tmp$subtitle <- sprintf("%d non-compliant, all observations = %d", 
                        sum(!is.na(tmp$data[, "prune_id"])), nrow(tmp$data))

# point plot --------------------------------------------------------------

tmp$title <- "Sales vs Material (Added Value) Validation"
tmp$ggp_point_addval <- tmp$data |>
  filter(sales_lga >= 0, mat_lga >= 0) |>
  mutate(prune_id = if_else(is.na(prune_id), "ok", prune_id)) |>
  ggp_valid_point(
    x_var = "mat_lga", y_var = "sales_lga", prune_var = "prune_id",
    colrs = list(prune = colrs$prune_class),
    titles = list(
      title = tmp$title, subtitle = tmp$subtitle, x = "Materials", y = "Sales")) +
  ggp_theme_valid(ggthemes::theme_hc(base_size = 12, base_family = "sans", 
                                     style = "darkunica"), 
                  legend_pos = c(0.8, 0.2))
# tmp$ggp_point_addval



tmp$title <- "Added Value vs Hours (Price) Validation"
tmp$ggp_point_price <- tmp$data |>
  filter(addval_lga >= 0, hrs_lga >= 0) |>
  mutate(prune_id = if_else(is.na(prune_id), "ok", prune_id)) |>
  ggp_valid_point(
    x_var = "hrs_lga", y_var = "addval_lga", prune_var = "prune_id",
    colrs = list(prune = colrs$prune_class),
    titles = list(
      title = tmp$title, subtitle = tmp$subtitle, x = "Hours", y = "Added Value")) +
  ggp_theme_valid(ggthemes::theme_hc(base_size = 12, base_family = "sans", 
                                     style = "darkunica"), 
                  legend_pos = c(0.8, 0.2))
# tmp$ggp_point_price


# non-compliant histograms ------------------------------------------------


tmp$title <- "Outliers' Total Sales by Group"
tmp$ggp_out_amt<- tmp$data |>
  ggp_valid_bar(
    x_var = "group", y_var = "sales", prune_var = "prune_id",
    stats_var = "tot",
    colrs = list(prune = colrs$prune_class),
    titles = list(title = tmp$title, subtitle = tmp$subtitle, y = "amount in $")) +
  ggthemes::theme_economist()
# tmp$ggp_out_amt


tmp$title_out_nb <- "Outliers' Total Numbers by Group"
tmp$ggp_out_nb <- tmp$data |>
  ggp_valid_bar(
    x_var = "group", y_var = "sales", prune_var = "prune_id",
    stats_var = "nb", 
    colrs = list(prune = colrs$prune_class),
    titles = list(title = tmp$title_out_nb,x = NULL, y = "count", fill = NULL)) +
  ggthemes::theme_economist()
# tmp$ggp_out_nb

tmp$ggp_noncompl_hist <- tmp$ggp_out_amt / tmp$ggp_out_nb
# tmp$ggp_noncompl_hist


# non-compliant tables ----------------------------------------------------

tmp$gt_noncompliant_df <- tmp$data |>
  filter(!is.na(prune_id)) |>
  # do the matches in 3 step to keep zero, oob together for gt later
  select(group, sales, matches("zero$"), matches("oob$"), matches("maha$")) |>
  select(-n_zero, -n_oob) |>
  group_by(group) |>
  summarize(across(.cols = !matches("group"), .fns = sum)) |>
  identity()

tmp$title <- sprintf("<em>%s<br>Non-compliant Total Sales & Nb by Group</em>", 
                     const$cie_name)
tmp$subtitle <- sprintf("<em>For %d observations</em>", nrow(tmp$data))
tmp$gt_noncompl <- tmp$gt_noncompliant_df |>
  gt_noncompliant(row_var = "group",
                  titles = list(title = tmp$title,
                                subtitle = tmp$subtitle))
# tmp$gt_noncompl

# assert ------------------------------------------------------------------

tmp$data |>
  chain_start() %>%
  assertr::verify(nrow(.) == tmp$data_n) |>
  chain_end()


# teardown ----------------------------------------------------------------

xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_addval", 
                     value = tmp$ggp_point_addval)
xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_price", 
                     value = tmp$ggp_point_price)
xprts_obj <- set_bag(xprts_obj, name = "ggp_noncompl_hist", 
                     value = tmp$ggp_noncompl_hist)
xprts_obj <- set_bag(xprts_obj, name = "gt_noncompl", 
                     value = tmp$gt_noncompl)



suppressWarnings(rm(tmp))
