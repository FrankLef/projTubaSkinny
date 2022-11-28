summ_corr <- function(data, corr_digits = 3, stats_digits = 2) {
  corr_df <- data |>
    corrr::correlate(quiet = TRUE) |>
    corrr::rearrange() |>
    corrr::shave() |>
    mutate(across(.cols = where(is.numeric),
                  .fns = round, digits = corr_digits))
  # remove first row which has all NAs
  corr_df <- corr_df[-1, ]
    
  
  stats <- list()
  stats$mean <- sapply(data, FUN = mean)
  stats$sd <- sapply(data, FUN = sd)
  stats$df <- bind_rows(stats$mean, stats$sd)|>
    mutate(across(.cols = where(is.numeric), 
                  .fns = round, digits = stats_digits))
  stats$df$term <- c("mean", "sd")
  stats$df <- stats$df[, names(corr_df)]
  
  assertthat::assert_that(identical(names(corr_df), names(stats$df)),
                          msg = "corr and stats df must have the same names.")
  
  bind_rows(corr_df, stats$df)
}