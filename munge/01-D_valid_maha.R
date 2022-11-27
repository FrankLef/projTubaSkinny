# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()


tmp$data <- get_data(sales) |>
  select(sales_lga, mat_lga, hrs_lga, n_false) |>
  mutate(across(.cols = c("sales_lga", "mat_lga", "hrs_lga"), 
                .fns = ~ if_else(n_false == 0, ., NA_real_))) |>
  select(-n_false) %>%  # must use %>%
  # mutate(across(.cols = c("sales", "mat", "hrs"), .fns = log)) %>%   # must use %>%
  mutate(maha_dist = assertr::maha_dist(., keep.NA = TRUE, robust = FALSE)) |>
  identity()
# glimpse(tmp$data)



# validator ---------------------------------------------------------------

tmp$validatr <- validate::validator(
  maha = maha_dist <= 9 * mad(maha_dist, na.rm = TRUE))



# confront ----------------------------------------------------------------

tmp$confront <- validate::confront(tmp$data, tmp$validatr)
tmp$confront
# summary(tmp$confront)
# plot(tmp$confront)

tmp$maha <- as.vector(values(tmp$confront))
# replace NAs with TRUE since they are not maha
# useful later when counting for gt tables
tmp$maha[is.na(tmp$maha)] <- TRUE
assertthat::assert_that(length(tmp$maha) == nrow(tmp$data),
                        is.logical(tmp$maha),
                        sum(!tmp$maha) == 8)

# append prune id to original data ----------------------------------------

tmp$data <- get_data(sales) |>
  mutate(maha_dist = tmp$data$maha_dist,
         maha = tmp$maha,
         prune_id = case_when(
           n_zero != 0 ~ "zero",
           n_oob != 0 ~ "oob",
           !maha ~ "maha",
           TRUE ~ NA_character_
         ))
tmp$prune_nb <- tmp$data |>
  count(prune_id)
assertthat::assert_that(
  sum(tmp$prune_nb$n[!is.na(tmp$prune_nb$prune_id)]) == 638)

# teardown ----------------------------------------------------------------


sales <- set_data(sales, data = tmp$data)


suppressWarnings(rm(tmp))


