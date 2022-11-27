# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)


# bootstrap ---------------------------------------------------------------

boot <- list()
boot$file <- file.path(getwd(), "data", "boot.qs")
boot$is_file <- TRUE
if (!boot$is_file) {
  message("The bootstrap takes about 2 min.")
  startTime <- Sys.time()
  
  boot$out <- boot_modl_lm(data = prep$train$baked, formula = model$forml,
                           times = 1000, apparent = TRUE, 
                           seed = as.integer(as.Date("2022-07-18")))
  
  boot$sampl <- boot$out |>
    select(-splits) |>
    unnest(models) |>
    select(id, term, estimate, std.error)
  
  endTime <- Sys.time()
  print(difftime(endTime, startTime))  # must print from inside of if()
  assertthat::assert_that(not_empty(boot))
  qsave(boot, file = boot$file)
} else {
  message(sprintf("Loading %s", basename(boot$file)))
  boot <- qread(boot$file)
}
assertthat::assert_that(not_empty(boot$out), not_empty(boot$sampl))
# glimpse(boot$out)


# coefs intervals ---------------------------------------------------------

boot$intrvl <- rsample::int_t(boot$out, statistics = models, alpha = 0.05)
assertthat::assert_that(nrow(boot$intrvl) == 28)
# glimpse(boot$intrvl)

# compare interval assuming normality (model) vs interval using
# t-like interval.
# IMPORTANT: The results are different significantly for the tems
#            that have a high p.value, i.e. unlikely to be statistically
#            meaningful.
boot$intrvl |>
  inner_join(model$intrvl, by = "term") |>
  mutate(est_diff = estimate - .estimate)

# coefs scatmat -----------------------------------------------------------

# ggp_boot <- list()
# 
# ggp_boot$df <- boot$sampl |>
#   filter(grepl(pattern = ".Intercept.|group.+:.+|^mat$|^hrs$", x = term))|>
#   select(-std.error)|>
#   pivot_wider(id_cols = id, names_from = term, values_from = estimate) |>
#   select(-id)
# 
# message("scatmat takes 5 sec. to draw")
# ggp_boot$scatMat <- GGally::ggscatmat(ggp_boot$df, alpha = 0.25)
# ggp_boot$scatMat



# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))

