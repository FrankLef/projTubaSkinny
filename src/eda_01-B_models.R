# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
fits <- list()

# lm models ---------------------------------------------------------------


fits$lm$forml <- list(
  "lm_0a" = sales ~ 1 + group,
  "lm_1a" = sales ~ 1 + group + mat,
  "lm_1b" = sales ~ 1 + group + hrs,
  "lm_2a" = sales ~ 1 + group + mat + hrs,
  "lm_2b" = sales ~ 1 + group + mat + hrs + mat:group + hrs:group,
  "lm_2c" = sales ~ 1 + group + mat + hrs + mat:group + hrs:group + mat:hrs)

fits$lm$out <- purrr::map(.x = fits$lm$forml, .f = function(x) {
  fit(linear_reg(engine = "lm"), formula = x, data = prep$train$baked)
})

fits$lm$results <- purrr::map_dfr(.x = fits$lm$out, .f = broom::glance, 
                                  .id = "model")
assertthat::assert_that(nrow(fits$lm$results) == length(fits$lm$forml))
fits$lm$results

# multilevel models -------------------------------------------------------

fits$lmr$forml <- list(
  # Null: Random intercepts
  "lmr_0a" = sales ~ 1 + (1 | group),
  # Random intercepts with fixed slope
  "lmr_1a" = sales ~ 1 + mat + (1 | group),
  # Random intercepts with fixed slope
  "lmr_1b" = sales ~ 1 + hrs + (1 | group),
  # Random intercepts with random slope
  "lmr_2a" = sales ~ 1 + mat + (1 + mat | group),
  # Random intercepts with random slope
  "lmr_2b" = sales ~ 1 + hrs + (1 + hrs | group),
  # Random intercepts, multivariable
  "lmr_3a" = sales ~ 1 + mat + hrs + (1 | group),
  # Random intercepts with covariate at level 2
  "lmr_3b" = sales ~ 1 + mat + hrs + mat:hrs + (1 | group)
  )

fits$lmr$out <- purrr::map(.x = fits$lmr$forml, .f = function(x) {
  fit(linear_reg(engine = "lmer"), formula = x, data = prep$train$baked)
})

fits$lmr$results <- purrr::map_dfr(.x = fits$lmr$out, .f = broom::glance, 
                                   .id = "model")
fits$lmr$results


# select best model -------------------------------------------------------

model <- list()
model$name <- "lm_2b"
model$forml <- fits$lm$forml[[model$name]]
model$best_fit <- fits$lm$out[[model$name]]
# tidy(model$best_fit)

model$intrvl <- tidy(model$best_fit) |>
  mutate(lower = estimate - 2 * std.error, 
         estimate = estimate,
         upper = estimate + 2 * std.error,
         alpha = 0.05,
         method = "normal") |>
  select(-statistic)
# model$intrvl

# predictions -------------------------------------------------------------

model$train$predict <-
  augment(model$best_fit, new_data = prep$train$baked)
# model$train$predict

model$test$predict <-
  augment(model$best_fit, new_data = prep$test$baked)
# model$test$predict
assert_that(nrow(prep$data) == nrow(model$train$predict) + 
              nrow(model$test$predict))

model$train$R2 <- yardstick::rsq(model$train$predict,
                                 truth = sales,
                                 estimate = .pred)$.estimate
model$train$rmse <- yardstick::rmse(model$train$predict,
                                 truth = sales,
                                 estimate = .pred)$.estimate
model$test$R2 <- yardstick::rsq(model$test$predict,
                                 truth = sales,
                                 estimate = .pred)$.estimate
model$test$rmse <- yardstick::rmse(model$test$predict,
                                truth = sales,
                                estimate = .pred)$.estimate
assert_that(model$train$R2 >= 0.75, model$test$R2 >= 0.75)
assert_that(model$train$rmse < 0.5, model$test$rmse < 0.5)

# invert predictions ------------------------------------------------------

# this step is used as a test for inverting values

# invert the data
model$train$predict <- model$train$predict |>
  mutate(sales_inv = invert_lognorm(model$train$predict$sales, 
                                    rec = prep$train$rec, 
                                    term = "sales", 
                                    id = c("log_out", "norm_out")))
assert_that(nrow(model$train$predict) == nrow(prep$train$split))
assert_that(sum(model$train$predict$sales_inv) == 
              sum(model$train$predict$sales_inv))


# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))

