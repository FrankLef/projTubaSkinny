library("ProjectTemplate")
# clear all variables except sticky_variables, see config file
ProjectTemplate::clear()

# scipen = 999: to prevent scientific notation
# dplyr.summarise.inform=F: to get rid of annoying message in summarize()
options(digits = 4, warn = 1, error = NULL, scipen = 999,
        dplyr.summarise.inform = FALSE)  # set project options

if (FALSE) {
  load.project()
} else {
  reload.project(override.config = list(data_ignore = 'extract_Access.R',
                                        cache_loading = TRUE,
                                        munging = TRUE,
                                        reset = TRUE))}
  # ProjectTemplate::cache("xprt")
  # ProjectTemplate::cache("sales")
  # ProjectTemplate::cache("sales_prun")
# }

# NOTE run cache() from time to time to verify the cache status
# clear.cache("qryR_sales_items.qs")

# run test directory
if (FALSE) testthat::test_dir(here("tests"))

# IMPORTANT
# ProjectTemplate does not source() with encoding = "UTF-8
# eflTools::run_eda(dir = here::here("lib"), pattern = "^help.+[.]R$")


# run the analysis scripts
if (TRUE) eflTools::run_eda()


# finalize ----------------------------------------------------------------

