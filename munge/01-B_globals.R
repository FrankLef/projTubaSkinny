# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

tmp$projects <- get_data(projects)

# colors ------------------------------------------------------------------

tmp$colrs_grps <- paletteer::paletteer_d("ggthemes::calc", direction = -1)
tmp$colrs_grps <- unclass(tmp$colrs_grps)
names(tmp$colrs_grps) <- unique(tmp$projects$client_grp1_id)
# tmp$colrs_grps

# teardown ----------------------------------------------------------------

projects <- set_info(projects, name = "colrs_client_grp1", value = tmp$colrs_grps)
projects

suppressWarnings(rm(tmp))


