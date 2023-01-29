
# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

# rename
tmp$ren <- c("id" = "project_id",
             "group" = "client_grp1_id", 
             "sales" = "vente_prix_tot",
             "mat" = "vente_achat",
             "hrs" = "jobs_work_hrs",
             "date" = "init_date_min")

# select, rename and add prune_var
tmp$data <- get_data(projects) |>
  select(tmp$ren) |>
  eflTools::ren_many(ren = tmp$ren) |>
  mutate(addval = sales - mat,
         period = year(date) * 100 + month(date),
         cap = const$cap_amt,
         labor = hrs * const$pay_rate) |>
  mutate(sales_lga = eflTools::log1ps(sales, base = 10),
         mat_lga = eflTools::log1ps(mat, base = 10),
         addval_lga = eflTools::log1ps(addval, base = 10),
         hrs_lga = eflTools::log1ps(hrs, base = 10),
         cap_lga = eflTools::log1ps(cap, base = 10),
         labor_lga = eflTools::log1ps(labor, base = 10))
# glimpse(tmp$data)


# validator ---------------------------------------------------------------


tmp$validatr <- validate::validator(
  id_uniq = is_unique(id),
  sales_na = !any(is.na(sales)),
  mat_na = !any(is.na(mat)),
  hrs_na = !any(is.na(hrs)),
  addval_na = !any(is.na(addval)),
  sales_zero = sales_lga >= 1,
  # ensure zero and oob are mutually exclusive
  sales_oob = in_range(sales_lga, 2, 5.5) | sales_lga < 1,
  # sales_oob = in_range(sales, 10, 5e5) | sales <= 1,
  mat_zero = mat_lga >= 1,
  # ensure zero and oob are mutually exclusive
  mat_oob = in_range(mat_lga, 2, 5.5) | mat_lga < 1,
  hrs_zero = hrs >= 1,
  # ensure zero and oob are mutually exclusive
  hrs_oob = in_range(hrs, 1.1, 3) | hrs < 1,
  cap_zero = cap_lga >= 1,
  labor_zero = labor_lga >= 1
  )


# confront ----------------------------------------------------------------

tmp$confront <- validate::confront(tmp$data, tmp$validatr, key = "id")
tmp$confront
# summary(tmp$confront)
# plot(tmp$confront)



# append result to data ---------------------------------------------------

# compute the nb of oob and zero.
# They must be mutually exclusive
tmp$file <- file.path(getwd(), "data", "confront_oob.qs")
# basename(tmp$file)

tmp$is_file <- TRUE
if (!tmp$is_file) {
  message("This takes 5 secs.")
  startTime <- Sys.time()
  tmp$confront_oob <- values(tmp$confront)[[1]] |>
    as.data.frame() |>
    rowwise() |>
    mutate(n_false = sum(!c_across(cols = everything())),
           n_zero = sum(!c_across(cols = ends_with("zero"))),
           n_oob = sum(!c_across(cols = ends_with("oob")))) |>
    identity()
  endTime <- Sys.time()
  print(difftime(endTime, startTime))  # must print from inside of if()
  qsave(tmp$confront_oob, file = tmp$file)
} else {
  message(sprintf("Loading %s", basename(tmp$file)))
  tmp$confront_oob <- qread(tmp$file)
}


# IMPORTANT: test n_false = z_zero + n_oob
#            The zero and oob rules must be mutually exclusive
# sum(tmp$confront_oob$n_false)
assertthat::assert_that(nrow(tmp$confront_oob) == nrow(tmp$data),
                        sum(tmp$confront_oob$n_false) == 1353)
assertthat::assert_that(sum(tmp$confront_oob$n_false - tmp$confront_df$n_zero -
                              tmp$confront_oob$n_oob) == 0,
                        msg = "Zero and oob rules must be mutually exclusive.")
# glimpse(tmp$confront_df)

tmp$data <- tmp$data |>
  bind_cols(tmp$confront_oob)


# teardown ----------------------------------------------------------------

sales <- bobj(tmp$data, formula = sales + addval ~ mat + hrs + date,
              id = "id")


suppressWarnings(rm(tmp))
