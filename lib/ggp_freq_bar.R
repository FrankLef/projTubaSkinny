ggp_freq_bar <- function(data, x_var, y_var, fill_var,
                         colrs = list(),
                         titles = list()) {
  data |>
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      breaks = scales::breaks_extended(),
      labels= scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = " M$")) +
    scale_fill_manual(values = colrs$grp) +
    labs(title = titles$title, subtitle = titles$subtitle,
         x = titles$x, y = titles$y) +
    facet_wrap(~ .data[[fill_var]], ncol = 1)
}

ggp_feq_bar_df <- function(data, x_var, y_var, fill_var) {
  data |>
    group_by(across(all_of(c(x_var, fill_var))))|>
    summarize(!!y_var := sum(.data[[y_var]])) |>
    mutate("{{y_var}}_lga" = eflTools::log_acctg(.data[[y_var]]))
}
