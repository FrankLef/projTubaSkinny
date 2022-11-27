# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

# prune the raw data
tmp$data <- get_data(sales) |>
  filter(is.na(prune_id)) |>
  select(-prune_id) |>
  calc_roi()
# glimpse(tmp$data)

tmp$data_n <- nrow(tmp$data)

# create dataframe of pruned data
tmp$data_noncompl <- get_data(sales) |>
  filter(!is.na(prune_id))
# glimpse(tmp$data_pruned)

assertthat::assert_that(nrow(tmp$data) + nrow(tmp$data_noncompl) ==
                          nrow(get_data(sales)))
tmp$colrs_names <- names(colrs$prune_id)[!is.na(names(colrs$prune_id))]
assertthat::assert_that(all(tmp$colrs_names %in% names(tmp$data_noncompl)))

tmp$colrs_grp1 <- get_info(projects, name = "colrs_client_grp1")


# data by group -----------------------------------------------------------


tmp$data_grp <- tmp$data |>
  select(group, sales, mat, hrs, labor, cap)

tmp$data_grp <- tmp$data_grp |>
  group_by(group) |>
  summarize(sales = sum(sales),
            mat = sum(mat),
            hrs = sum(hrs),
            labor = sum(labor),
            cap = mean(cap)) |>
  calc_roi() |>
  mutate(sales_pct = sales / sum(sales)) |>
  verify(near(sum(sales_pct), 1))


tmp$data_grp_tot <- tmp$data_grp |>
  select(-group)
tmp$data_grp_tot <- sapply(tmp$data_grp_tot, FUN = sum)
tmp$data_grp_tot <- data.frame(
  group = "Total",
  as.data.frame(t(tmp$data_grp_tot))) |>
  mutate(cap = mean(tmp$data_grp$cap)) |>
  calc_roi()
# tmp$data_grp_tot

tmp$data_grp <- tmp$data_grp |>
  bind_rows(tmp$data_grp_tot)
# tmp$data_grp


# visualize ---------------------------------------------------------------


tmp$title <- "Sales vs Material Costs"
tmp$subtitle <- sprintf("%d projects", nrow(tmp$data))
tmp$ggp_valid_sales <- tmp$data |>
  filter(hrs_lga >= 0, addval_lga >= 0) |>
  ggp_valid_clean(x_var ="sales_lga", y_var = "mat_lga", color_var ="group",
                  colrs = list(grp1 = tmp$colrs_grp1),
                  titles = list(title = tmp$title, subtitle = tmp$subtitle,
                                x = "Material", y = "Sales")) +
  ggp_theme_valid_clean(ggthemes::theme_tufte(), legend_pos = c(0.2, 0.8))
tmp$ggp_valid_sales


tmp$title <- "Added Value vs Hours (Price per hour)"
tmp$ggp_valid_price <- tmp$data |>
  filter(hrs_lga >= 0, addval_lga >= log10(50)) |>
  ggp_valid_clean(x_var ="hrs_lga", y_var = "addval_lga", color_var ="group",
                  colrs = list(grp1 = tmp$colrs_grp1),
                  list(title = tmp$title, subtitle = tmp$subtitle,
                       x = "Hours", y = "Added Value")) +
  ggp_theme_valid_clean(ggthemes::theme_tufte(), legend_pos = c(0.8, 0.2))
tmp$ggp_valid_price

# teardown ----------------------------------------------------------------

# update sales and add non-compliant and group data
sales <- set_data(sales, data = tmp$data)
sales <- eflTools::set_bag(sales, name ="noncompl", value = tmp$data_noncompl)
sales <- eflTools::set_bag(sales, name ="groups", value = tmp$data_grp)
# sales

xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_clean_addval", 
                     value = tmp$ggp_valid_sales)
xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_clean_price", 
                     value = tmp$ggp_valid_price)

suppressWarnings(rm(tmp))

