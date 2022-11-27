# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()


tmp$data <- get_data(sales)
# check nb of rows
tmp$data_n <- nrow(tmp$data)

tmp$subtitle <- sprintf("%d observations.", nrow(tmp$data))


# boxplot -----------------------------------------------------------------


tmp$title <- "Prix par dollar de mat\U00E9riaux selon les projets"
tmp$ggp_box_sales2mat <- tmp$data|>
  ggp_valid_box(x_var = "group", y_var = "sales2mat",
                colrs = list(grps = colrs_grps, box_clr = "blue"),
                titles = list(
                  title = tmp$title, subtitle = tmp$subtitle,
                  x = NULL, y = "sales / material")) +
  theme(plot.background = element_rect(fill = "oldlace"),
        legend.background = element_rect(fill = "oldlace"),
        title = element_text(color = "midnightblue"))
# tmp$ggp_box_sales2mat



tmp$title <- "Prix par heure selon les projets"
tmp$ggp_box_addval2hrs <- tmp$data |>
  ggp_valid_box(x_var = "group", y_var = "addval2hrs",
                colrs = list(grps = colrs_grps, box_clr = "blue"),
                titles = list(title = tmp$title, subtitle = tmp$subtitle,
                              x = NULL, y = "added value / hr")) +
  coord_cartesian(ylim = c(3, 3000)) +
  theme(plot.background = element_rect(fill = "oldlace"),
        legend.background = element_rect(fill = "oldlace"),
        title = element_text(color = "midnightblue"))
# tmp$ggp_box_addval2hrs


# table -------------------------------------------------------------------


# tmp$sales2mat_stats <- eflMunchr::summ_boxplot(tmp$data, box_var = "sales2mat",
#                                      group_var = "group",
#                                      is_log = FALSE) |>
#   mutate(ratio = "sales2mat")
# tmp$sales2mat_stats

tmp$sales2mat_stats <- tmp$data |>
  group_by(group) |>
  dplyr::summarise(boxplot = list(stats::setNames(
    grDevices::boxplot.stats(sales2mat, coef = 1.5)$stats,
    c('low_whisk','low_hinge','med','high_hinge','high_whisk')))) |>
  tidyr::unnest_wider(boxplot) |>
  mutate(ratio = "sales2mat")
# tmp$sales2mat_stats



# tmp$addval2hrs_stats <- eflMunchr::summ_boxplot(tmp$data, box_var = "addval2hrs",
#                                                group_var = "group",
#                                                is_log = FALSE) |>
#   mutate(ratio = "addval2hrs")
# tmp$addval2hrs_stats

tmp$addval2hrs_stats <- tmp$data |>
  group_by(group) |>
  dplyr::summarise(boxplot = list(stats::setNames(
    grDevices::boxplot.stats(addval2hrs, coef = 1.5)$stats,
    c('low_whisk','low_hinge','med','high_hinge','high_whisk')))) |>
  tidyr::unnest_wider(boxplot) |>
  mutate(ratio = "addval2hrs")
# tmp$addval2hrs_stats




tmp$stats <- tmp$sales2mat_stats |>
  rbind(tmp$addval2hrs_stats)
# tmp$stats

tmp$title <- sprintf("<em>%s<br>Price List - 2022</em>", const$cie_name)
tmp$subtitle <- "<em>Prices per dollar of material and hour of work</em>"
tmp$gt_groups_ratios <- tmp$stats |>
  select(-low_whisk, -high_whisk) |>
  pivot_longer(cols = all_of(c("low_hinge", "med", "high_hinge"))) |>
  gt_make_spanner(row_var = "group", col_var = "name", spann_var = "ratio",
                  val_var = "value", delim = ".") |>
  gt_groups_ratios(titles = list(title = tmp$title, subtitle = tmp$subtitle)) |>
  identity()
# tmp$gt_groups_ratios


# assert ------------------------------------------------------------------


tmp$data %>%
  chain_start() %>%
  assertr::verify(nrow(.) == tmp$data_n) %>%
  chain_end()
# identity()


# teardown ----------------------------------------------------------------

# xprt$ggp$sales2mat_box <- tmp$ggp_box_sales2mat
# xprt$ggp$addval2hrs_box <- tmp$ggp_box_addval2hrs
# xprt$gt$groups_ratios <- tmp$gt_groups_ratios

xprts_obj <- set_bag(xprts_obj, name = "ggp_sales2mat_box", 
                     value = tmp$ggp_box_sales2mat)
xprts_obj <- set_bag(xprts_obj, name = "ggp_addval2hrs_box", 
                     value = tmp$ggp_box_addval2hrs)
xprts_obj <- set_bag(xprts_obj, name = "gt_groups_ratios", 
                     value = tmp$gt_groups_ratios)


suppressWarnings(rm(tmp))
