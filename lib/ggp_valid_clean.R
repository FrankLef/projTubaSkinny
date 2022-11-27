ggp_valid_clean<- function(data, x_var, y_var, color_var, 
                           colrs = list(), titles = list()) {
  data |>
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]], 
               color = .data[[color_var]]))+
    geom_point(shape = 19, size = 1) +
    geom_smooth(method ="lm", se = FALSE) +
    scale_x_continuous(
      breaks = breaks_extended(),
      labels = function(x) label_number(accuracy = 1)(exp_acctg(x))) +
    scale_y_continuous(
      breaks = breaks_extended(),
      labels = function(x) label_number(accuracy = 1)(exp_acctg(x))) +
    scale_color_manual(values = colrs$grp1)+
    annotation_logticks(base = 10, sides = "bl") +
    labs(title = titles$title, subtitle = titles$subtitle,
         x = titles$x, y = titles$y,color = NULL)
}
