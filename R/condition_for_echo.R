condition_for_echo <- function(layout,
                               which_plate,
                               .well_col = "well",
                               .compound_col = "compound",
                               .concentration_col = "concentration",
                               .volume_col = "volume",
                               daughter_names = c("Destination Well", "compound", "daughter_conc", "daughter_final_vol"),
                               mother_names  = c("Source Well", "compound", "mother_conc")
) {

  # ensure valid plate type selection, or return error
  assertthat::assert_that(which_plate %in% c("mother", "daughter"))

  # update the column names and columns to be selected according to plate selected, or return error
  col_names <- switch(which_plate, "daughter" = daughter_names, "mother" = mother_names)
  which_cols <- switch(which_plate,
                       "daughter" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}, {{ .volume_col }}),
                       "mother" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}))

  # ensure all requested columns are in the layout, or return error
  assertthat::assert_that(length(col_names) == length(which_cols))

  # ensure length of new name vector matches selected columns, or return error
  assertthat::assert_that(all(as.character(which_cols) %in% names(layout)))

  # condition the layout
  layout %>%
    filter(is.na({{ .compound_col }}) == FALSE) %>%
    select(all_of(which_cols)) %>%
    set_names(col_names)

}
