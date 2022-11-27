ggp_theme_intervl_boot <- function(theme = ggthemes::theme_hc()) {
  theme +
    ggplot2::theme(title = ggplot2::element_text(color = "midnightblue"))
}

ggp_theme_intervl_dist <- function(theme = ggthemes::theme_hc(), legend_pos) {
  theme +
    theme(
      title = element_text(color = "midnightblue"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.background = element_rect(fill ="transparent"),
      plot.background = element_rect(fill = "lightgrey"))
}

ggp_theme_roi_balloon <- function(theme = ggthemes::theme_clean(), legend_pos, 
                                  colrs = list()){
  theme +
    theme(title = element_text(color = "midnightblue"),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = colrs$bg))
}

ggp_theme_valid <- function(theme, legend_pos) {
  theme +
    theme(title = element_text(color = "floralwhite"),
          legend.position = legend_pos,
          axis.title.y = element_text(angle = 90),
          strip.background = element_rect(fill = "darkgrey"))
}

ggp_theme_valid_clean <- function(theme = ggthemes::theme_tufte(), legend_pos) {
  theme +
    theme(title = element_text(color ="midnightblue"),
          axis.title = element_text(color ="midnightblue"),
          legend.position = legend_pos,
          legend.background = element_rect(fill = "transparent"))
}
