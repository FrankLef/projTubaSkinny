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

# data cuts ---------------------------------------------------------------

# names(tmp$data)
# tmp$data |> skimr::skim(addval2cap, addval2hrs, hrs2cap, sales2addval,
#                         profit2cap, profit2sales, sales2cap, sales2mat)
tmp$data <- tmp$data |>
  mutate(profit2sales_cut = 
           santoku::chop_equally(x = profit2sales, groups = 10), 
         addval2hrs_cut = 
           santoku::chop_equally(addval2hrs, groups = 10))
# glimpse(tmp$data)

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


# teardown ----------------------------------------------------------------

# update sales and add non-compliant and group data
sales <- set_data(sales, data = tmp$data)
sales <- eflTools::set_bag(sales, name ="noncompl", value = tmp$data_noncompl)
sales <- eflTools::set_bag(sales, name ="groups", value = tmp$data_grp)
# sales


suppressWarnings(rm(tmp))

