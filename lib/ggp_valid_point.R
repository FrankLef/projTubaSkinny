ggp_valid_point <- function(data, x_var, y_var, prune_var = "prune_id",
                            colrs = list(), titles = list()) {
  data |>
    ggplot(mapping = aes(x = .data[[x_var]], y = .data[[y_var]],
                         color = .data[[prune_var]], 
                         size = (.data[[prune_var]] == "ok"))) +
    geom_point(shape = 19) +
    scale_x_continuous(
      breaks = breaks_extended(),
      labels = function(x) label_number(accuracy = 1)(exp_acctg(x))) +
    scale_y_continuous(
      breaks = breaks_extended(),
      labels = function(x) label_number(accuracy = 1)(exp_acctg(x))) +
    scale_color_manual(values = colrs$prune) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 2), guide = "none") +
    annotation_logticks(base = 10, sides = "bl") +
    labs(title = titles$title, subtitle = titles$subtitle,
         x = titles$x, y = titles$y, color = NULL)
}
