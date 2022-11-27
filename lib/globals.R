
# constants ---------------------------------------------------------------

const <- list()

const$cie_name <- "Cookbook Project"
const$cap_amt <- 19000000  # total capitalisation
const$pay_rate <- 38.5  # average pay rate, including withholding taxes


# exports -----------------------------------------------------------------

# list to hold object to be exported
# xprt <- list(
#   ggp = list(),
#   ply = list(),
#   gt = list())

# colors ------------------------------------------------------------------

# list to hold the colors specs
colrs <- list()

# group colors
# colrs$grps <- paletteer::paletteer_d("ggthemes::calc", direction = -1)
# colrs$grps <- unclass(colrs$grps)
# names(colrs$grps) <- unique(projects$client_grp1_id)

# prune class colors
colrs$prune_class <- c("ok" = "ghostwhite", "zero" = "yellow", "oob" = "limegreen", 
                    "maha" = "red")

# prune id colors
colrs$prune_id <- paletteer::paletteer_d("ggsci::lanonc_lancet", direction = 1)
colrs$prune_id <- unclass(colrs$prune_id)
names(colrs$prune_id) <- c("sales_zero", "sales_oob", "mat_zero", "mat_oob",
                            "hrs_zero", "hrs_oob", "maha")

# cluters colors
colrs$roi_clust <- paletteer::paletteer_d("khroma::bright", direction = 1)
colrs$roi_clust <- unclass(colrs$roi_clust)
names(colrs$roi_clust) <- c("1", "2", "3", "4")
