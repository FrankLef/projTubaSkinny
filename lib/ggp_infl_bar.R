ggp_infl_bar <- function(data, x_var = "group", y_var = "num_tot",
                            labs = ggplot2::labs(title = "Title"),
                            colrs = list(grps = NULL)) {
  
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = .data[[x_var]], y = .data[[y_var]], 
                  label = scales::label_dollar()(.data[[y_var]])),
              vjust = 1.5, fontface = "bold", color = "darkblue") +
    scale_y_continuous(breaks = breaks_extended(),
                       labels = label_number_auto()) +
    scale_fill_manual(values = colrs$grps) +
    ggthemes::theme_solarized_2() +
    labs
}
