# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_data(sales)

# roi sales ---------------------------------------------------------------

tmp$ggp_addval_clust <- tmp$data|>
  ggplot(aes(x = addval2hrs, y = hrs2cap, color = clust)) +
  geom_point() +
  scale_color_manual(values = colrs$roi_clust,
                     labels = c("Low V High P", "High V Low P",
                                "High V High P", "High V High P")) +
  ggthemes::theme_hc() +
  theme(plot.background = element_rect(fill = "oldlace"),
        legend.background = element_rect(fill = "oldlace"),
        title = element_text(color = "midnightblue"))
tmp$ggp_addval_clust

tmp$ggp_addval_clust_log <- tmp$data |>
  ggplot(aes(x = hrs2cap, y = addval2hrs, color = clust)) +
  geom_point(size = 2) +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = scales::breaks_log(),
                     labels = scales::label_log()) +
  scale_y_continuous(trans = scales::log_trans(),
                     breaks = scales::breaks_log(),
                     labels = scales::label_log()) +
  scale_color_manual(values = colrs$roi_clust,
                     labels = c("Low V High P", "High V Low P", 
                                "High V High P", "High V High P")) +
  ggthemes::theme_hc() +
  theme(plot.background = element_rect(fill = "oldlace"),
        legend.background = element_rect(fill = "oldlace"),
        title = element_text(color = "midnightblue")) +
  labs(title = "Price per hr vs Volume",
       subtitle = sprintf("%d projets", nrow(tmp$data)),
       x = "Hours per 1 dollar of capital (log scale)",
       y = "Added Value in dollars per hour (log scale)")
tmp$ggp_addval_clust_log


tmp$ggp_profit_clust_log <- tmp$data|>
  ggplot(aes(x = sales2cap, y = profit2sales, color = clust)) +
  geom_point(size = 2) +
  scale_x_continuous(trans = scales::log_trans(),
                     breaks = scales::breaks_log(),
                     labels = scales::label_log()) +
  scale_y_continuous(trans = scales::log_trans(),
                     breaks = scales::breaks_log(),
                     labels = scales::label_log()) +
  scale_color_manual(values = colrs$roi_clust,
                     labels = c("Low V High P", "High V Low P", 
                                "High V High P", "High V High P")) +
  ggthemes::theme_hc() +
  theme(plot.background = element_rect(fill = "oldlace"),
        legend.background = element_rect(fill = "oldlace"),
        title = element_text(color = "midnightblue")) +
  labs(title = "Profit per Sales vs Volume",
       subtitle = sprintf("%d projets", nrow(tmp$data)),
       x = "Sales per 1 dollar of capital (log scale)",
       y = "Profit per Sales (log scale)")
tmp$ggp_profit_clust_log


# assert ------------------------------------------------------------------


# tmp$data %>%
#   chain_start() %>%
#   assertr::verify(nrow(.) == tmp$data_n) %>%
#   chain_end()
# identity()


# teardown ----------------------------------------------------------------

# xprt$ggp$addval_clust <- tmp$ggp_addval_clust
# xprt$ggp$addval_clust_log <- tmp$ggp_addval_clust_log
# xprt$ggp$profit_clust_log <- tmp$ggp_profit_clust_log

xprts_obj <- set_bag(xprts_obj, name = "ggp_addval_clust", 
                     value = tmp$ggp_addval_clust)
xprts_obj <- set_bag(xprts_obj, name = "ggp_addval_clust_log", 
                     value = tmp$ggp_addval_clust_log)
xprts_obj <- set_bag(xprts_obj, name = "ggp_profit_clust_log", 
                     value = tmp$ggp_profit_clust_log)

suppressWarnings(rm(tmp))
