# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)
# tmp$data


# matrix summaries --------------------------------------------------------

tmp$corr_summ <- tmp$data |>
  select(sales_lga, mat_lga, hrs_lga) |>
  summ_corr(is_rearr = TRUE, corr_digits = 3, stats_digits = 3)
# tmp$corr_summ

tmp$gt_corr_summ <- tmp$corr_summ |>
  gt_corr(row_var = "term", group_var = NULL,
          terms = c("mean", "sd"),
          colrs = list(pal = "oompaBase::jetColors"),
          titles = list(title = "Statistics Summary")) |>
  gt::opt_stylize(style = 6, color = "blue")
tmp$gt_corr_summ



# matrix summaries by group -----------------------------------------------

tmp$corr_summ_grp <- tmp$data |>
  select(group, sales_lga, mat_lga, hrs_lga) |>
  group_by(group) |>
  group_modify(.f = ~ summ_corr(., is_rearr = FALSE, 
                                corr_digits = 3, stats_digits = 2), 
            .keep = FALSE) |>
  identity()
# tmp$corr_summ_grp


tmp$gt_corr_summ_grp <- tmp$corr_summ_grp |>
  gt_corr(row_var = "term", group_var = "group",
          terms = c("mean", "sd"),
          colrs = list(pal = "oompaBase::jetColors"),
          titles = list(title = "Statistics Summary by Group")) |>
  gt::opt_stylize(style = 6, color = "blue")
tmp$gt_corr_summ_grp


# teardown ----------------------------------------------------------------


xprts_obj <- set_bag(xprts_obj, name = "gt_corr_summ",
                     value = tmp$gt_corr_summ)
xprts_obj <- set_bag(xprts_obj, name = "gt_corr_summ_grp",
                     value = tmp$gt_corr_summ_grp)

suppressWarnings(rm(tmp))

