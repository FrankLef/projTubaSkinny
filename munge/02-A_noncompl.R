# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)
tmp <- new.env()

# convert flags to character
tmp$data_noncompliant <- get_bag(sales, name = "noncompl") |>
  select(id, group, period, sales_zero, sales_oob, mat_zero, mat_oob,
                        hrs_zero, hrs_oob, maha)
# check count
tmp$data_n <- nrow(tmp$data_noncompliant)
# str(tmp$data_noncompliant)


# create non-compliant factor ---------------------------------------------

# create long format
tmp$data_noncompliant_long <- tmp$data_noncompliant |>
  pivot_longer(cols = c("sales_zero", "sales_oob",
                        "mat_zero", "mat_oob",
                        "hrs_zero", "hrs_oob",
                        "maha"),
               names_to = "prune_id") |>
  filter(!is.na(value), !value) |>
  mutate(group = as.factor(group),
         prune_id = as.factor(prune_id))
assertthat::assert_that(
  all(levels(tmp$data_noncompliant_long$prune_id) %in% names(colrs$prune_id)))
# glimpse(tmp$data_noncompliant_long)


# Plot the tree -----------------------------------------------------------

# There is a lot of info in the source. read it through before googling
# SOURCE:
# This is the best source, very complete examples
# https://cran.r-project.org/web/packages/ggparty/vignettes/ggparty-graphic-partying.html
# This one is also a good illustration on how to do it
# https://luisdva.github.io/rstats/Plotting-conditional-inference-trees-in-R/

# fit the tree
tmp$tree <- partykit::ctree(data = tmp$data_noncompliant_long,
                          formula = prune_id ~ group + period)
assertthat::assert_that(inherits(tmp$tree, what = "party"))
# plot(tmp$tree)

# t <- partykit::nodeapply(tmp$tree, ids = "terminal")

# to get the prediction for each row you can do this
# glimpse(predict(tmp$tree))

# create the ggplot object
# tmp$ggp_tree <- ggparty::ggparty(tmp$tree)
# assertthat::assert_that(is.ggplot(tmp$ggp_tree))

# title
tmp$title <- "Decision Tree of Non-Compliant Projects"
# subtitle
tmp$subtitle <- "%d non-compliant items in %d projects"
tmp$subtitle <- sprintf(tmp$subtitle, nrow(fitted(tmp$tree)), 
                        nrow(tmp$data_noncompliant))
tmp$ggp_noncompl_tree <- ggp_noncompliant_tree(tmp$tree,
                                      fill_var = "prune_id", 
                                      fill_colrs = colrs$prune_id)
# tmp$ggp_noncompl_tree


# teardown ----------------------------------------------------------------

# xprt$ggp$noncompl_tree <- tmp$ggp_noncompl_tree

xprts_obj <- set_bag(xprts_obj, name = "ggp_noncompl_tree", 
                     value = tmp$ggp_noncompl_tree)

suppressWarnings(rm(tmp))
