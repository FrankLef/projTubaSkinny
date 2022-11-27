boot_modl_lm <- function(data, formula, times = 1000, apparent = TRUE,
                         seed = NULL) {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 2)
  checkmate::assertFormula(formula)
  checkmate::assertCount(times, positive = TRUE)
  checkmate::assertFlag(apparent)
  checkmate::assertInt(seed)
  
  # define the function
  func <- function(split, formula = formula, ...) {
    out <- fit(linear_reg(engine = "lm"), 
               formula = formula, 
               data = split) |>
      tidy() |>
      as.data.frame()
    
    sel_mat <- out$term %in% c("groupBomb:mat", "groupGE:mat", "groupOther:mat",
                               "groupZimmer:mat", "groupZodiac:mat")
    sel_hrs <- out$term %in% c("groupBomb:hrs", "groupGE:hrs", "groupOther:hrs",
                               "groupZimmer:hrs", "groupZodiac:hrs")
    est_mat <- out$estimate[sel_mat]
    est_hrs <- out$estimate[sel_hrs]
    var_mat <- out$std.error[sel_mat]^2
    var_hrs <- out$std.error[sel_hrs]^2
    df <- 
      data.frame(
        term = c("matBomb", "matGE", "matOther", "matZimmer", "matZodiac",
                 "hrsBomb", "hrsGE", "hrsOther", "hrsZimmer", "hrsZodiac"),
        estimate = c(
          est_mat + out$estimate[out$term == "mat"],
          est_hrs + out$estimate[out$term == "hrs"]),
        # IMPORTANT: the intercept and slopes are independent of each other
        #            therefore no need to add 2 * cov to the variances.
        std.error = c(
          sqrt(var_mat + out$std.error[out$term == "mat"]^2),
          sqrt(var_hrs + out$std.error[out$term == "hrs"]^2)),
        statistic = NA_real_,
        p.value = NA_real_)
    out <- out |> 
      bind_rows(df)
    out
    }
  
  set.seed(seed)
  rsample::bootstraps(data, times = times, apparent = apparent) |>
    mutate(models = purrr::map(splits, function(split) {
      df <- as.data.frame(split)
      func(df, formula)
    }))
}
