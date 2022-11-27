ggp_noncompliant_tree <- function(obj, fill_var = "prune_id", fill_colrs) {
  checkmate::assertClass(obj, "party")
  
  # format the tree nodes
  node_plot <- list(geom_bar(aes(x = "", fill = .data[[fill_var]]),
                                 position = position_fill()),
                    scale_fill_manual(values = fill_colrs),
                    theme_void(),
                    labs(x = NULL, y = NULL, fill = NULL))
  # label nodes with ID, split variable and p value
  node_label_inner_line <- list(aes(label = paste("Node", id)),
                                    aes(label = splitvar))
  # set graphical parameters for each line
  node_label_inner_gpar <- list(list(size = 8, col = "black", fontface = "bold"),
                                    list(size = 12))
  # create the tree plot
  ggparty::ggparty(obj, terminal_space = 1/3) +
    geom_edge() +
    geom_edge_label(color = "darkblue", size = 4) +
    # labels for inner nodes
    geom_node_label(aes(label = splitvar),
                    line_list = node_label_inner_line,
                    line_gpar = node_label_inner_gpar,
                    ids = "inner") +
    # labels for terminal nodes
    geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
                    fontface = "bold",
                    ids = "terminal",
                    size = 3,
                    # 0.01 nudge_y is enough to be above the node plot since a terminal
                    # nodeplot's top (not center) is at the node's coordinates.
                    nudge_y = 0.01) +
    # geom_node_splitvar() +
    geom_node_plot(gglist = node_plot,
                   ids = "terminal",
                   shared_axis_labels = TRUE,
                   shared_legend = TRUE,
                   legend_separator = FALSE) +
    theme(title = element_text(color = "midnightblue"))
}
