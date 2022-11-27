invert_lognorm <- function(x, rec, term = "sales", id = c("log", "norm")) {
  checkmate::assertNumeric(x, finite = TRUE)
  checkmate::assertClass(rec, "recipe")
  checkmate::assertString(term, min.chars = 1)
  checkmate::assertCharacter(id, min.chars = 1, len = 2)
  
  specs <- list()
  
  # get the log specs
  specs$log <- recipes::tidy(rec, id = id[1]) |>
    filter(.data$terms == term) |>
    pull(.data$base) |>
    setNames(nm = "base")
  
  # get the center/normalize specs
  specs$norm <- recipes::tidy(rec, id = id[2]) |>
    filter(.data$terms == term) |>
    pull(.data$value)
  len <- length(specs$norm)
  if (len == 2) {
    specs$norm <- setNames(specs$norm, nm = c("mean", "sd"))
  } else if (len == 1) {
    specs$norm <- c("mean" = specs$norm, "sd" = 1)  # set sd to 1 if not used
  } else {
    msg <- sprintf("%d is an invalid nb of items.", len)
    stop(msg)
  }
  
  y <- x * specs$norm["sd"] + specs$norm["mean"]
  specs$log["base"] ^ y
}

invert_lognorm_intrvl <- function(data, rec, 
                              rec_var = "rec_id", 
                              vars = c(".estimate", ".lower", ".upper"),
                              vars_inv = c("est_inv", "low_inv", "upr_inv")) {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 3)
  checkmate::assertClass(rec, "recipe")
  checkmate::assertNames(rec_var, subset.of = names(data))
  checkmate::assertNames(vars, subset.of = names(data))
  checkmate::assertCharacter(vars_inv, len = length(vars))
  checkmate::assertNames(vars_inv, type = "strict")
  
  
  for (i in seq_along(vars)) {
    var <- vars[i]
    var_inv <- vars_inv[i]
    data <- data |>
      mutate({{var_inv}} := case_when(
        .data[[rec_var]] == "sales" ~ 
          invert_lognorm(.data[[var]], rec = rec, term = "sales", 
                         id = c("log_out", "norm_out")),
        .data[[rec_var]] == "mat" ~ 
          invert_lognorm(.data[[var]], rec = rec, term = "mat", 
                         id = c("log_pred", "norm_pred")),
        .data[[rec_var]] == "hrs" ~ 
          invert_lognorm(.data[[var]], rec = rec, term = "hrs", 
                         id = c("log_pred", "norm_pred")),
        TRUE ~ NA_real_))
    }
  assert_that(!any(is.na(data[, vars_inv[1]])),
              !any(is.na(data[, vars_inv[2]])),
              !any(is.na(data[, vars_inv[3]])))
  data
}
