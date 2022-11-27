ggp_intervl_range <- function(data, term_var = "term", est_var = ".estimate", 
                        low_var = ".lower", upr_var = ".upper", 
                        est_label = "est_label") {
  
  ggplot(data, aes(x = .data[[est_var]], y = .data[[term_var]], 
             xmin = .data[[low_var]], xmax = .data[[upr_var]])) +
    ggplot2::geom_pointrange(size = 1, fatten = 2, color = "royalblue",
                             position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::geom_vline(xintercept = 0, color = "magenta",
                        linetype = "solid", linewidth = 3, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(x = .data[[est_var]], y = .data[[term_var]],
                                 label = scales::label_number_auto()(.data[[est_label]])),
                             size = 3, color = "navy") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_extended()) +
    ggplot2::scale_color_brewer(palette = "Dark2")
}
