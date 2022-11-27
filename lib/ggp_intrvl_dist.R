ggp_intrvl_dist <- function(data, data_stats, x_var = "estimate", y_var = "term",
                            width = c(0.67, 0.95, 1), hline_term = "mat",
                            hline_var = ".estimate") {
  checkmate::assertDataFrame(data, min.rows = 2, min.cols = 2)
  checkmate::assertDataFrame(data_stats, min.rows = 2, min.cols = 2)
  checkmate::assertNames(x_var, subset.of = names(data))
  checkmate::assertNames(y_var, subset.of = names(data))
  checkmate::assertNumeric(width, lower = 0.01, upper = 1, unique = TRUE)
  checkmate::assertString(hline_term, min.chars = 1)
  checkmate::assertNames(hline_var, subset.of = names(data_stats))
  
  # IMPORTANT SOURCE: No the recommended way has changed and is described
  #                   in the following source
  # https://cran.r-project.org/web/packages/ggdist/vignettes/slabinterval.html
  
  hline_val <- data_stats[data_stats$term == hline_term, hline_var, drop = TRUE]
  assertthat::assert_that(is.numeric(hline_val))
  
  text_form <- list(size = 4, color = "magenta")
  
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    stat_halfeye(aes(fill = after_stat(
      ggdist::cut_cdf_qi(cdf, .width = width))),
      color = "black") +
    ggrepel::geom_text_repel(data_stats,
                             mapping = aes(x = .data$estimate, 
                                           y = .data$term,
                                           label = round(.data$est_inv, 0)),
                             size = text_form$size, color = text_form$color, 
                             fontface = "bold") +
    ggrepel::geom_text_repel(data_stats,
                             mapping = aes(x = .data$.lower, 
                                           y = .data$term,
                                           label = round(.data$low_inv, 0)),
                             size = text_form$size, color = text_form$color) +
    ggrepel::geom_text_repel(data_stats,
                             mapping = aes(x = .data$.upper, 
                                           y = .data$term,
                                           label = round(.data$upr_inv, 0)),
                             size = text_form$size, color = text_form$color) +
    ggplot2::geom_vline(xintercept = hline_val, color = "brown", linetype = "dashed",
                        linewidth = 1, alpha = 0.5) +
    scale_fill_paletteer_d("fishualize::Hypsypops_rubicundus", direction = 1)
}
