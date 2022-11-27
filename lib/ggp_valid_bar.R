ggp_valid_bar <- function(data, x_var = "group", y_var = "amt",
                          prune_var = "prune_id",
                          stats_var = c("tot", "nb"),
                          colrs = list(), titles = list()) {
  stats_var <- match.arg(stats_var)

  data_out <- data |>
    ggp_valid_bar_df(x_var = x_var, y_var = y_var, prune_var = prune_var)

  
  data_out |> 
    ggplot(aes(x = .data[[x_var]], y = .data[[stats_var]], 
               fill = .data[[prune_var]])) +
    geom_bar(stat = "identity") +
    geom_text_repel(aes(
      label = scales::label_number_auto()(.data[[stats_var]])), 
                    color = "darkblue", fontface = "bold", size = 3) +
    scale_y_continuous(breaks = breaks_extended(),
                       labels = label_number_auto()) +
    scale_fill_manual(values = colrs$prune) +
    labs(title = titles$title, subtitle = titles$subtitle,
         x = titles$x, y = titles$y, fill = NULL)
}

ggp_valid_bar_df <- function(data, x_var, y_var,prune_var) {
  data |>
    filter(!is.na(.data[[prune_var]])) |>
    group_by(.data[[x_var]], .data[[prune_var]]) |>
    summarize(nb = n(),
              tot = sum(.data[[y_var]])) |>
    ungroup() |>
    mutate(pct = tot / sum(tot))
}
