prune_box <- function(data, num_var, box_var, group_var = NULL, id = "out", 
                      prune_var = "prune_id", is_log = TRUE, coef = 1.5) {
  checkmate::assertDataFrame(data, min.cols = 2, min.rows = 1)
  checkmate::assertNames(num_var, subset.of = names(data))
  checkmate::assertNames(box_var, subset.of = names(data))
  checkmate::assertString(group_var, min.chars = 1, null.ok = TRUE)
  checkmate::assertNames(prune_var, subset.of = names(data))
  checkmate::assertString(id, min.chars = 1)
  checkmate::assertFlag(is_log)
  checkmate::assertNumber(coef, lower = 0L)
  
  # stats by group, excluding pruned rows, to find outliers by group
  data_box <- eflMunchr::summ_boxplot(data, group_var = group_var, 
                                      box_var = box_var, 
                                      prune_var = prune_var,
                                      is_log = is_log,
                                      coef = coef)
  
  if (is.null(group_var)) {
    # source: https://stackoverflow.com/questions/8753531/repeat-rows-of-a-data-frame-n-times
    data_extra <- do.call("rbind", replicate(nrow(data), data_box, simplify = FALSE))
    
    # add stats to data
    out <- data |>
      dplyr::bind_cols(data_extra)
  } else {
    out <- data |>
      dplyr::inner_join(y = data_box, by = group_var)
  }
  
  # prune the data
  out |>
    mutate(!!prune_var := case_when(
      is.na(.data[[prune_var]]) & ((.data[[num_var]] < .data$low_whisk) | 
                                     (.data[[num_var]] > .data$high_whisk)) ~ !!id,
      TRUE ~ .data[[prune_var]]))
}
