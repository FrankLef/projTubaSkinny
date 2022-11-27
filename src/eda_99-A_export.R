# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)

tmp <- new.env()

tmp$path <- here::here("reports", "data")
# tmp$path

# eflTools::xprt_ggplot(xprts_obj, path = tmp$path, verbose = TRUE)
# eflTools::xprt_gt(xprts_obj, path = tmp$path, verbose = TRUE)

eflTools::xprt_all(xprts_obj, path = tmp$path, is_xprt = FALSE)


# finalize ----------------------------------------------------------------

rm(tmp)
