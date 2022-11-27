# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$data <- get_bag(sales,"groups")
# tmp$data

# table of revenues and profits -------------------------------------------

tmp$title <- sprintf("<em>%s<br>Revenus & Cost by Group</em>", const$cie_name)
tmp$subtitle <- sprintf("<em>For %d observations, in thousands of dollars</em>", tmp$data_n)

tmp$gt_groups_amt <- tmp$data |>
  filter(!grepl(pattern = "^total", group, ignore.case = TRUE)) |>
  select(group, sales, mat, hrs, addval, labor, profit, cap) |>
  gt_groups_amt(row_var = "group",
                titles = list(title = tmp$title, subtitle = tmp$subtitle))
# tmp$gt_groups_amt

# table of ROI ------------------------------------------------------------


tmp$data_roi <- tmp$data |>
  select(-sales, -mat, -hrs, -addval, -labor, -profit, -cap,
         -sales2addval, -sales2mat, -sales_pct)

tmp$title <- sprintf("<em>%s<br>Return On Investments Ratios</em>", const$cie_name)
tmp$subtitle <- sprintf("<em>For %d observations</em>", tmp$data_n)

tmp$gt_groups_roi <- tmp$data_roi |>
  gt_groups_roi(row_var = "group",
                titles = list(title = tmp$title, subtitle = tmp$subtitle))
# tmp$gt_groups_roi



# ggplot group ROI --------------------------------------------------------


tmp$title <- "Return On Investment by Group"
tmp$subtitle <- "Using added value as the return definition"
tmp$ggp_groups_roi_addval <- tmp$data_roi |>
  filter(!grepl("^total", x = group, ignore.case = TRUE)) |>
  ggp_roi_balloon(x_var = "hrs2cap", y_var = "addval2hrs", 
                  z_var = "addval2cap", group_var = "group",
                  colrs = list(groups = colrs_grps),
                  titles = list(title = tmp$title, subtitle = tmp$subtitle,
                                x = "Volume ratio", y = "Price per hour")) +
  ggp_theme_roi_balloon(ggthemes::theme_clean(), legend_pos = c(0.8, 0.8), 
                        colrs = list(bg = "seashell"))
# tmp$ggp_groups_roi_addval

tmp$title <- "Return On Investment by Group"
tmp$subtitle <- "Using profit as the return definition"
tmp$ggp_groups_roi_profit <- tmp$data_roi |>
  filter(!grepl("^total", x = group, ignore.case = TRUE)) |>
  ggp_roi_balloon(x_var = "sales2cap", y_var = "profit2sales", 
                  z_var = "profit2cap", group_var = "group",
                  colrs = list(groups = colrs_grps),
                  titles = list(title = tmp$title, subtitle = tmp$subtitle,
                                x = "Volume ratio", y = "Profit margin")) +
  ggp_theme_roi_balloon(ggthemes::theme_clean(), legend_pos = c(0.8, 0.2), 
                        colrs = list(bg = "snow"))
# tmp$ggp_groups_roi_profit


# plotly group ROI --------------------------------------------------------


tmp$ply_groups_roi_df <- tmp$data_roi |>
  filter(!grepl("^total", x = group, ignore.case = TRUE)) |>
  mutate(addval2cap_label = scales::label_percent(accuracy = 0.1)(addval2cap),
         profit2cap_label = scales::label_percent(accuracy = 0.1)(profit2cap))
tmp$ply_groups_roi_df

tmp$title <-"Return On Investment by Group"
tmp$subtitle <- "Using added value as the return definition"
tmp$title <- paste(tmp$title, tmp$subtitle, sep = "\n")
tmp$ply_groups_roi_addval <- tmp$ply_groups_roi_df |>
  ply_roi_balloon(x_var = "hrs2cap", y_var = "addval2hrs", 
                  size_var = "addval2cap", 
                  text_var = "addval2cap_label", 
                  color_var ="group",
                  legend_pos= c(0.8, 0.9),
                  colrs = list(bg = "seashell", grp1 = colrs_grps), 
                  titles = list(title = tmp$title, 
                                x = "Volume ratio",
                                y = "Price per hour"))
# tmp$ply_groups_roi_addval


tmp$title <-"Return On Investment by Group"
tmp$subtitle <- "Using profit as the return definition"
tmp$title <- paste(tmp$title, tmp$subtitle, sep = "\n")
tmp$ply_groups_roi_profit<- tmp$ply_groups_roi_df |>
  ply_roi_balloon(x_var = "sales2cap", y_var = "profit2sales", 
                  size_var = "profit2cap", 
                  text_var = "profit2cap_label", 
                  color_var ="group",
                  legend_pos= c(0.8, 0.9),
                  colrs = list(bg = "seashell", grp1 = colrs_grps), 
                  titles = list(title = tmp$title, 
                                x = "Volume ratio",
                                y = "Profit margin"))
# tmp$ply_groups_roi_profit



# teardown ----------------------------------------------------------------


xprts_obj <- set_bag(xprts_obj, name = "ggp_groups_roi_addval", 
                     value = tmp$ggp_groups_roi_addval)
xprts_obj <- set_bag(xprts_obj, name = "ggp_groups_roi_profit", 
                     value = tmp$ggp_groups_roi_profit)
xprts_obj <- set_bag(xprts_obj, name = "gt_groups_amt", 
                     value = tmp$gt_groups_amt)
xprts_obj <- set_bag(xprts_obj, name = "gt_groups_roi", 
                     value = tmp$gt_groups_roi)
xprts_obj <- set_bag(xprts_obj, name = "ply_groups_roi_addval", 
                     value = tmp$ply_groups_roi_addval)
xprts_obj <- set_bag(xprts_obj, name = "ply_groups_roi_profit", 
                     value = tmp$ply_groups_roi_profit)

suppressWarnings(rm(tmp))

