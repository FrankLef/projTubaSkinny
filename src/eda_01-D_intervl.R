# setup -------------------------------------------------------------------
# stop(paste("MANUAL STOP", basename(whereami::thisfile())), call. = FALSE)

# data --------------------------------------------------------------------

intrvl <- list()

intrvl$normal <- model$intrvl |>
  mutate(method = "normal")
# str(intrvl$normal)

intrvl$studt <- boot$intrvl |>
  mutate(method = "student-t") |>
  rename(estimate = .estimate,
         lower = .lower,
         upper = .upper)
# str(intrvl$studt)

intrvl$mean_qi <- boot$sampl |>
  group_by(term) |>
  ggdist::mean_qi(estimate, .width = c(0.67, 0.95)) |>
  mutate(method = "mean_qi") |>
  rename(lower = .lower,
         upper = .upper)
# str(intrvl$mean_qi)

intrvl$all <- intrvl$normal[, c("method", "term", "lower", "estimate", "upper")] |> 
  bind_rows(intrvl$studt[, c("method", "term", "lower", "estimate", "upper")]) |>
  bind_rows(intrvl$mean_qi[intrvl$mean_qi$.width == 0.95, 
                           c("method", "term", "lower", "estimate", "upper")])
assert_that(nrow(intrvl$all) == 18 + 28 + 28)


ggplot(intrvl$all, aes(x = estimate, y = term, 
                 xmin = lower, xmax = upper,
                 color = method)) +
  ggplot2::geom_pointrange(size = 1, fatten = 2,
                           position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_vline(xintercept = 0, color = "magenta",
                      linetype = "solid", linewidth = 3, alpha = 0.5) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_extended()) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggthemes::theme_hc() +
  labs(title = "Comparing the methods of intervals",
       x = NULL, y = NULL)

msg <- "Comparing intervals: mean_qi can be used to vizualise student-t."
cat(msg, "\n")

# teardown ----------------------------------------------------------------

suppressWarnings(rm(tmp))

