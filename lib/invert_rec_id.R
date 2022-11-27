invert_rec_id <- function(data, term_var = "term", id_var = "rec_id") {
  data |>
    mutate({{id_var}} := case_when(
      grepl("mat", x = .data[[term_var]]) ~ "mat",
      grepl("hrs", x = .data[[term_var]]) ~ "hrs",
      TRUE ~ "sales"))
}
