ggp_valid_box <- function(data, x_var = "group", y_var = "ratio", 
                          colrs = list(grps = NULL, box_clr = "black"),
                          titles = list()) {
  
  data_stats_lng <- suppressWarnings(
    ggp_valid_box_stats(data, x_var = x_var, y_var = y_var, 
                        is_log = TRUE, coef = 1.5))

  # data used for horizontal lines
  the_hlines <- data |>
    summarize(med = median(.data[[y_var]]),
              Q25 = quantile(.data[[y_var]], probs = 0.25),
              Q75 = quantile(.data[[y_var]], probs = 0.75)) |>
    pivot_longer(cols = everything()) |>
    mutate(x = -Inf,
           label = scales::label_number(accuracy = 0.01)(value),
           color = c("lightgreen", "hotpink", "hotpink"))
  
  
  
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
    geom_boxplot(color = colrs$box_clr) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 4,
                 color = "darkblue", fill = "darkblue") +
    geom_text_repel(data = data_stats_lng,
                    mapping = aes(x = group, y = value, label = label),
                    inherit.aes = FALSE,
                    size = 3.5, color = colrs$box_clr) +
    geom_hline(data = the_hlines, aes(yintercept = value),
               color = the_hlines$color, linetype = "dashed", linewidth = 1) +
    geom_label_repel(data = the_hlines,
                     mapping = aes(x = x, y = value, label = label),
                     inherit.aes = FALSE,
                     direction = "y",
                     size = 4, color = "darkblue",
                     fill = the_hlines$color) +
    scale_y_continuous(trans = scales::log_trans(),
                       breaks = breaks_log(n = 7, base = 10),
                       labels = label_number(accuracy = 0.01)) +
    scale_fill_manual(values = colrs$grps) +
    labs(title = titles$title, subtitle = titles$subtitle,
         x = titles$x, y = titles$y, fill = NULL)
}


ggp_valid_box_stats <- function(data, x_var = "group", y_var = "ratio", 
                                prune_var = NULL, is_log = TRUE, coef = 1.5) {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 2)
  checkmate::assertNames(x_var, subset.of = names(data))
  checkmate::assertNames(y_var, subset.of = names(data))
  checkmate::assertFlag(is_log)
  
  # prune the row if a prune variable is provided
  if (!is.null(prune_var)) {
    checkmate::assertNames(prune_var, subset.of = names(data))
    data <- data |>
      filter(is.na(.data[[prune_var]]))
  }
  
  # convert y to log if required
  if(is_log) data <- data |> 
    mutate({{y_var}} := log(.data[[y_var]]))
  
  # group by group if a grouping variable is provided
  if (!is.null(x_var)) {
    data <- data |>
      group_by(.data[[x_var]])
  }
  
  # the column names for the boxplot table
  the_names <- c("low_whisk", "low_hinge", "med", "upr_hinge", "upr_whisk")
  if (is_log) the_names <- paste(the_names, "log", sep = "_")
  
  # SOURCE: very good source with more good details
  # https://stackoverflow.com/questions/56669653/boxplot-stats-in-dplyr-with-groups
  out <- data |>
    dplyr::summarise(boxplot = list(stats::setNames(
      grDevices::boxplot.stats(.data[[y_var]], coef = coef)$stats,
      the_names))) |>
    tidyr::unnest_wider(.data$boxplot)
  
  # if log = TRUE then do the inverse with exp
  if (is_log) {
    out <- out |>
      mutate(
        low_whisk = exp(.data$low_whisk_log),
        low_hinge = exp(.data$low_hinge_log),
        med = exp(.data$med_log),
        high_hinge = exp(.data$upr_hinge_log),
        high_whisk = exp(.data$upr_whisk_log))
    }
  
  # put stats by group in long format for ggrepel
  out |>
    select(group, low_whisk, low_hinge, med, high_hinge, high_whisk) |>
    pivot_longer(cols = !matches(x_var)) |>
    mutate(label = scales::label_number(accuracy = 0.01)(value))
}
