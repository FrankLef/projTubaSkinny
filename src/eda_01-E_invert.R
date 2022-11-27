# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)


# invert model coefs ------------------------------------------------------

# identify the terms used to invert
boot$intrvl <- invert_rec_id(boot$intrvl, term_var = "term", id_var = "rec_id")
# boot$intrvl

boot$intrvl <- invert_lognorm_intrvl(boot$intrvl, 
                                     rec = prep$train$rec, 
                                     rec_var = "rec_id", 
                                     vars = c(".estimate", ".lower", ".upper"),
                                     vars_inv = c("est_inv", "low_inv", "upr_inv"))
boot$intrvl

model$intrvl <- invert_rec_id(model$intrvl, term_var = "term", id_var = "rec_id")
model$intrvl

model$intrvl <- invert_lognorm_intrvl(model$intrvl, 
                                     rec = prep$train$rec, 
                                     rec_var = "rec_id", 
                                     vars = c("estimate", "lower", "upper"),
                                     vars_inv = c("est_inv", "low_inv", "upr_inv"))
model$intrvl

# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))

