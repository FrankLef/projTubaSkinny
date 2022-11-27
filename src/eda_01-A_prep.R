# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
prep <- list()

prep$data <- get_data(sales) |>
  select(id, sales, group, mat, hrs, clust,
         clust_label) |>
  mutate(group = as.factor(group),
         clust = as.factor(clust),
         clust_label = as.factor(clust_label))


# split -------------------------------------------------------------------

set.seed(as.integer(as.Date("2022-07-12")))
prep$split <- prep$data |>
  initial_split(strata = group)

prep$train$split <- training(prep$split)
prep$test$split <- testing(prep$split)
assertthat::assert_that(nrow(prep$data) == 
                          nrow(prep$train$split) + nrow(prep$test$split))

# recipe ------------------------------------------------------------------

prep$rec <- recipe(
  sales ~ ., data = prep$data) |>
  update_role(id, new_role = "idx") |>
  # to avoid error at bake
  update_role_requirements("idx", bake = FALSE) |>
  step_log(all_outcomes(), id = "log_out") |>
  step_log(all_numeric_predictors(), id = "log_pred") |>
  step_normalize(all_outcomes(), id = "norm_out") |>
  step_normalize(all_numeric_predictors(), id = "norm_pred") |>
  check_missing(everything(), id = "missing")
# summary(prep$rec)
# tidy(prep$rec)

# compute the estimates used to bake later
prep$train$rec <- recipes::prep(prep$rec, training = prep$train$split)
prep$test$rec <- recipes::prep(prep$rec, training = prep$test$split)
# tidy(prep$train$rec)

# data to use for inverse
# tidy(prep$train$rec, id = "log_out")
# tidy(prep$train$rec, id = "log_pred")
# tidy(prep$train$rec, id = "norm_out")
# tidy(prep$train$rec, id = "norm_pred")

prep$train$baked <- recipes::bake(prep$train$rec, prep$train$split)
prep$test$baked <- recipes::bake(prep$test$rec, prep$test$split)
assertthat::assert_that(nrow(prep$data) == 
                          nrow(prep$train$baked) + nrow(prep$test$baked))
# glimpse(prep$data$train)


# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))

