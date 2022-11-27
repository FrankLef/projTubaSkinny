# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

# prune the raw data
tmp$data <- get_data(sales)
# str(tmp$data)


# clusters ----------------------------------------------------------------

tmp$data_pca <- tmp$data |>
  select(sales, mat, hrs, labor, addval2hrs, addval2cap, profit2sales, profit2cap) |>
  FactoMineR::PCA(scale.unit = TRUE, ncp = 3, graph = FALSE)
# summary(tmp$data_pca)


# SOURCE: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/119-pca-in-r-using-ade4-quick-scripts/

# visualize the eigen values
# fviz_eig(tmp$data_pca)

# visualize the individual observations
fviz_pca_ind(tmp$data_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label = "none")     # Avoid text overlapping)



# get the coordinate for each data point in terms of principal components
# str(tmp$data_pca$ind$coord)

# get the clusters
tmp$data_hcpc <- tmp$data_pca |>
  FactoMineR::HCPC(graph = FALSE)
# summary(tmp$data_hcpc)


# names(tmp$data_hcpc)
# str(tmp$data_hcpc$data.clust)

# SOURCE: http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization#cluster-analysis-and-factoextra

# visualize the individual observaitons by cluster
factoextra::fviz_cluster(tmp$data_hcpc,
                         show.clust.cent = TRUE, # Show cluster centers
                         palette = "jco",         # Color palette see ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         main = "Factor map",
                         repel = TRUE)

# str(tmp$data_hcpc$data.clust)

# create the cluster data
tmp$data_clust <- tmp$data_hcpc$data.clust |>
  mutate(id = tmp$data$id,
         group = tmp$data$group,
         addval2hrs = tmp$data$addval2hrs,
         hrs2cap = tmp$data$hrs2cap,
         profit2sales = tmp$data$profit2sales,
         sales2cap = tmp$data$sales2cap) |>
  # NOTE: See the next (recursive) step to see where the labels come from
  mutate(clust_label = case_when(
    clust == "1" ~ "Low V, High P",
    clust == "2" ~ "High V, Low P",
    clust == "3" ~ "High V, High P",
    clust == "4" ~ "High V, High P",
    TRUE ~ NA_character_)) |>
  mutate(group = as.factor(group)) |>
  relocate(group) |>
  relocate(id)
assertthat::assert_that(nrow(tmp$data_clust) == nrow(tmp$data))
assertthat::assert_that(
  all(levels(tmp$data_clust$clust) %in% names(colrs$roi_clust)))

# glimpse(tmp$data_clust)


# clusters labels ---------------------------------------------------------

# NOTE: The 2 plots here are to visualize the clusters and label them in the
#       previous step just above.

# visualize the clusters
tmp$ggp_addVal_clust_log <- ggplot(tmp$data_clust,
                                   aes(x = hrs2cap, y = addval2hrs, 
                                       color = clust)) +
  geom_point() +
  scale_x_continuous(
    trans = scales::log_trans(),
    breaks = scales::breaks_log(),
    labels = scales::label_log()
  ) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::breaks_log(),
    labels = scales::label_log()
  ) +
  scale_color_manual(values = colrs$roi_clust,
                     labels = c("Low V High P", "High V Low P", 
                                "High V High P", "High V High P")) +
  coord_fixed(ratio = 9/16) +
  theme_minimal() +
  labs(x = "volume = hrs2cap", y = "price = addval2hrs")
# tmp$ggp_addVal_clust_log


# visualize the clusters
tmp$ggp_profit_clust_log <- ggplot(tmp$data_clust,
                                   aes(x = sales2cap, y = profit2sales, 
                                       color = clust)) +
  geom_point() +
  scale_x_continuous(
    trans = scales::log_trans(),
    breaks = scales::breaks_log(),
    labels = scales::label_log()
  ) +
  scale_y_continuous(
    trans = scales::log_trans(),
    breaks = scales::breaks_log(),
    labels = scales::label_log()
  ) +
  scale_color_manual(values = colrs$roi_clust,
                     labels = c("Low V High P", "High V Low P", 
                                "High V High P", "High V High P")) +
  coord_fixed(ratio = 9/16) +
  theme_minimal() +
  labs(x = "volume = sales2cap", y = "price = profit2sales")
tmp$ggp_profit_clust_log

# append clusters to sales ------------------------------------------------

tmp$data <- tmp$data |>
  mutate(clust = tmp$data_clust$clust,
         clust_label = tmp$data_clust$clust_label)

# str(tmp$data)

# teardown ----------------------------------------------------------------

# new data set with clusters
sales <- set_data(sales, data = tmp$data)

xprts_obj <- set_bag(xprts_obj, name = "ggp_profit_clust_log", 
                     value = tmp$ggp_profit_clust_log)


suppressWarnings(rm(tmp))
