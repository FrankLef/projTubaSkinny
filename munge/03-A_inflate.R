# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)
# glimpse(tmp$data)

tmp$scale <- 1000L

# quantile(tmp$data$sales, probs = 0:5 / 5) %/% tmp$scale


# inflate -----------------------------------------------------------------

tmp$data_infl <- tmp$data |>
  mutate(w = .data[["sales"]] %/% tmp$scale) |>
  uncount(weights = w, .id = "infl_id") |>
  identity()



# sales 2 mat -------------------------------------------------------------

tmp$title <- "Prix par dollar de mat\U00E9riaux selon les ventes"
tmp$ggp_box_sales2mat <- ggp_valid_box(tmp$data_infl, 
                                        x_var = "group", y_var = "sales2mat",
                                        colrs = list(grps = colrs_grps,
                                                     box_clr = "black")) +
  labs(
    title = tmp$title, subtitle = tmp$subtitle,
    y = "sales / material", x = NULL, fill = NULL) +
  ggthemes::theme_solarized_2()
# tmp$ggp_box_sales2mat

# addval 2 hrs ------------------------------------------------------------

tmp$title <- "Valeur ajoutee par heure selon les ventes"
tmp$ggp_ratio_sales_hrs <- ggp_valid_box(tmp$data_infl, 
                                       x_var = "group", y_var = "addval2hrs",
                                       colrs = list(grps = colrs_grps,
                                                    box_clr = "black")) +
  labs(
    title = tmp$title, subtitle = tmp$subtitle,
    y = "added value / hr", x = NULL, fill = NULL) +
  ggthemes::theme_solarized_2()
# tmp$ggp_ratio_sales_hrs


# sales by group ----------------------------------------------------------


tmp$data_grp_stats <- tmp$data |>
  group_by(group) |>
  summarize(nb = n(),
            tot = sum(sales)) |>
  mutate(tot = round(tot, 0))
# tmp$data_grp_stats



tmp$ggp_tot <- ggp_infl_bar(tmp$data_grp_stats, x_var = "group", y_var = "tot",
                            labs = ggplot2::labs(title = "Ventes par groupe",
                                                 x = NULL, fill = NULL),
                            colrs = list(grps = colrs_grps))
# tmp$ggp_tot

# tmp$ggp_ratio / tmp$ggp_tot

# assert ------------------------------------------------------------------


# tmp$data %>%
#   chain_start() %>%
#   assertr::verify(nrow(.) == tmp$data_n) %>%
#   chain_end()
# identity()


# teardown ----------------------------------------------------------------

# new
sales <- set_bag(sales, name ="inflated", value = tmp$data_infl)


suppressWarnings(rm(tmp))
