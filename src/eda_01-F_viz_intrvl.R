# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)

# coefs intervals ---------------------------------------------------------

ggp_intervl_sampl <- ggp_intervl_range(boot$intrvl, 
                                term_var = "term", est_var = ".estimate", 
                                low_var = ".lower", upr_var = ".upper",
                                est_label = "est_inv") +
  labs(title = "Coefficients' Intervals as per Bootstrap",
       x = NULL, y = NULL) +
  ggp_theme_intervl_boot(ggthemes::theme_hc())
# ggp_intervl_sampl



# dist data ---------------------------------------------------------------

ggp_dist_nice <- list()
ggp_dist_nice$data <- boot$sampl |>
  filter(grepl(pattern = "^hrs|^mat", term)) 
# ggp_dist_nice$data

# get 67% intervals for labels
ggp_dist_nice$labels <- ggp_dist_nice$data |>
  select(-id, -std.error) |>
  group_by(term) |>
  ggdist::mean_qi(estimate, .width = 0.67) |>
  identity()
ggp_dist_nice$labels <- invert_rec_id(ggp_dist_nice$labels, 
                                      term_var = "term", id_var = "rec_id")
# ggp_dist_nice$labels
# compute inverted values
ggp_dist_nice$labels <- invert_lognorm_intrvl(ggp_dist_nice$labels, 
                                              rec = prep$train$rec, 
                                              rec_var = "rec_id", 
                                              vars = c("estimate", ".lower", ".upper"),
                                              vars_inv = c("est_inv", "low_inv", "upr_inv"))
# ggp_dist_nice$labels


# coefs distribution - Hours ----------------------------------------------


# get selected rows from data data
ggp_dist_nice$hrs <- ggp_dist_nice$data |>
  filter(grepl(pattern = "^hrs", term))
# ggp_dist_nice$hrs

# get intervals for labels
ggp_dist_nice$hrs_labels <- ggp_dist_nice$labels |>
  filter(rec_id == "hrs")
# ggp_dist_nice$hrs_labels

ggp_dist_nice$hrs_plot <- ggp_intrvl_dist(ggp_dist_nice$hrs, ggp_dist_nice$hrs_labels,
                                     x_var = "estimate", y_var = "term",
                                     width = c(0.67, 0.95, 1),
                                     hline_term = "hrs",
                                     hline_var = "estimate") +
  labs(title = "Distribution of Price per Hour",
       subtitle = "With intervals at 67% and 95%",
       x = "Logarithmic scale (To allow comparisons)", y = NULL, fill = NULL) +
  ggp_theme_intervl_dist(ggthemes::theme_hc(), legend_pos = c(0.9, 0.8))
ggp_dist_nice$hrs_plot


# coefs distribution - Material -------------------------------------------

# get selected rows from data data
ggp_dist_nice$mat <- ggp_dist_nice$data |>
  filter(grepl(pattern = "^mat", term))
# ggp_dist_nice$mat

# get intervals for labels
ggp_dist_nice$mat_labels <- ggp_dist_nice$labels |>
  filter(rec_id == "mat")
# ggp_dist_nice$mat_labels


ggp_dist_nice$mat_plot <- ggp_intrvl_dist(ggp_dist_nice$mat, ggp_dist_nice$mat_labels,
                                     x_var = "estimate", y_var = "term",
                                     width = c(0.67, 0.95, 1),
                                     hline_term = "mat",
                                     hline_var = "estimate") +
  labs(title = "Distribution of Price per Dollar of Material",
       subtitle = "With intervals at 67% and 95%",
       x = "Logarithmic scale (To allow comparisons)", y = NULL, fill = NULL) +
  ggp_theme_intervl_dist(ggthemes::theme_hc(), legend_pos = c(0.9, 0.8))
ggp_dist_nice$mat_plot


# coefs tableau -----------------------------------------------------------

gt_intrvl <- ggp_dist_nice$labels |>
  select(rec_id, term, est_inv, low_inv, upr_inv) |>
  filter(!grepl(pattern = ":", x = term)) |>
  arrange(rec_id, term) |>
  gt_groups_intrvl(
    rowgroup_var = "rec_id", row_var = "term",
    titles = list(
      title = sprintf("<em>%s<br>Price Model - 2022</em>", const$cie_name),
      subtitle = "<em>Prices per dollars of material and hours of work</em>"
    )
  )
# gt_intrvl

# teardown ----------------------------------------------------------------


xprts_obj <- set_bag(xprts_obj, name = "ggp_intervl_sampl", 
                     value = ggp_intervl_sampl)
xprts_obj <- set_bag(xprts_obj, name = "ggp_dist_nice_hrs", 
                     value = ggp_dist_nice$hrs_plot)
xprts_obj <- set_bag(xprts_obj, name = "ggp_dist_nice_mat", 
                     value = ggp_dist_nice$mat_plot)
xprts_obj <- set_bag(xprts_obj, name = "gt_intrvl", 
                     value = gt_intrvl)

# xprt$ggp$intervl_sampl <- ggp_intervl_sampl
# xprt$ggp$dist_nice_hrs <- ggp_dist_nice$hrs_plot
# xprt$ggp$dist_nice_mat <- ggp_dist_nice$mat_plot
# xprt$gt$intrvl <- gt_intrvl

suppressWarnings(rm(tmp))

