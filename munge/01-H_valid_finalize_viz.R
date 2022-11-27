# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)
# glimpse(tmp$data)

tmp$colrs_grp1 <- get_info(projects, name = "colrs_client_grp1")


# scatter plots -----------------------------------------------------------


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

xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_clean_addval", 
                     value = tmp$ggp_valid_sales)
xprts_obj <- set_bag(xprts_obj, name = "ggp_valid_clean_price", 
                     value = tmp$ggp_valid_price)

suppressWarnings(rm(tmp))

