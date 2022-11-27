# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)


# predict -----------------------------------------------------------------

ggp_predict <- list()
ggp_predict$train$title <- "Training Data"
ggp_predict$train$subtitle <- sprintf("R2 = %.1f%%, RMSE = %.4f, n = %d", 
                                      model$train$R2 * 100, model$train$rmse,
                                      nrow(model$train$predict))
ggp_predict$train$plot <- ggplot(model$train$predict,
                                 aes(x = sales, y = .pred, color = group)) +
  geom_point() +
  scale_color_manual(values = colrs_grps) +
  labs(title = ggp_predict$train$title,
       subtitle = ggp_predict$train$subtitle,
       x = "sales", y = "prediction")
# ggp_predict$train$plot

ggp_predict$test$title <- "Test Data"
ggp_predict$test$subtitle <- sprintf("R2 = %.1f%%, RMSE = %.4f, n = %d", 
                                     model$test$R2 * 100, model$test$rmse,
                                     nrow(model$test$predict))
ggp_predict$test$plot <- ggplot(model$test$predict,
                                aes(x = sales, y = .pred, color = group)) +
  geom_point() +
  scale_color_manual(values = colrs_grps) +
  labs(title = ggp_predict$test$title,
       subtitle = ggp_predict$test$subtitle,
       x = "sales", y = "prediction")
# ggp_predict$test$plot

ggp_predict$train$plot + ggp_predict$test$plot +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Comparing predicted vs observed values") &
  ggthemes::theme_economist() &
  theme(legend.title = element_blank(),
        legend.position = "bottom")


# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))
