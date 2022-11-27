# setup -------------------------------------------------------------------
stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)
# tmp$data


# matrix summaries --------------------------------------------------------

tmp$stats <- tmp$data |>
  select(sales_lga, mat_lga, hrs_lga) |>
  summarize(across(.cols = everything(), .fns = list(mean = mean, sd = sd))) |>
  identity()
# tmp$stats


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

