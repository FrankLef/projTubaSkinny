# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

# prune the raw data
tmp$ok <- "ok"
tmp$data <- get_data(sales) |>
  mutate(prune_id = if_else(is.na(prune_id), tmp$ok, prune_id))

# plotly 3D ---------------------------------------------------------------

tmp$subtitle <- sprintf("%d non-compliant, all observations = %d",
                        sum(tmp$data[, "prune_id"] != tmp$ok), nrow(tmp$data))
tmp$title <- paste0("Original Data With Colored Non-Compliant Observations", "\n",
                    tmp$subtitle)

tmp$ply_valid <-  tmp$data |>
  ply_valid_3D(x_var = "mat", y_var = "hrs", z_var = "sales", 
               color_var = "prune_id", 
               colrs = list(prune = colrs$prune_class, grid ="gainsboro"), 
               titles = list(title = tmp$title, 
                             x = 'Material',y = 'Hours', z ='Sales'))
tmp$ply_valid


# teardown ----------------------------------------------------------------

# xprt$ply$valid_3D <- tmp$ply_valid

xprts_obj <- set_bag(xprts_obj, name = "ply_valid_3D", 
                     value = tmp$ply_valid)

suppressWarnings(rm(tmp))
