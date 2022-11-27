ggp_roi_balloon <- function(data, x_var, y_var, z_var, group_var = "group",
                            colrs = list(), titles =list()) {
  ggplot(data,
         mapping = aes(x = .data[[x_var]], y = .data[[y_var]], 
                       size = .data[[z_var]], color = .data[[group_var]])) +
    geom_point() +
    scale_color_manual(values = colrs$groups) +
    scale_size_area(max_size = 30, guide = "none") +
    ggrepel::geom_text_repel(aes(label = 
                                   scales::label_percent(accuracy = 0.1)(.data[[z_var]])), 
                             size = 6, fontface = "bold", color = "midnightblue") +
    labs(title = titles$title,subtitle = titles$dubtitle,
         x = titles$x,y = titles$y)
  }
