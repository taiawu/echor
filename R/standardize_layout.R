#' Standardize layouts for echor functions
#'
#' A helper function to standardize contents and column names of plate layouts for use in downstream echor package functions. Users can provide the names of the necessary columns, relieving the previous requirement that layouts have precisely-names variables to be used with these functions.
#'
#' @param layout A plate layout, as read in by dsfworld::read_plate_layout(). This layout must contain the following information: well, compound, final transferred volume (in nL), final concentration of the transferred solution. These columns may have any names in the provided layout; this function will look for the names "compound", "concentration" and "volume" by default. The layout can have any number of additional variables; these additional variables may be plotted using the plate-visualization functions in the echor package, but are not necessary to create the echo instructions file.
#' @param which_plate Choose "mother" to standardize a mother layout. Choose "daughter" to standardize a daughter layout.
#' @param .well_col name of the column in the layout containing well names. Defaults to "well".
#' @param .compound_col name of the column containing concentration information. Defaults to "compound". These compound names are the only information used to pair the correct source and desintations wells in the mother and daughter plates, so idential compounds must have the same name in mother and daughter plates, and different compounds cannot have the same name.
#' @param .concentration_col name of the column in the layout containing concentration information. For mother plates, this should be the concentration of the solution in the mother plate. e.g. if the mother plate contains a 10 mM DMSO stock in well A1, the concentration for well A1 should be 10). For daughter plates, this should be the concentration of the final solution transferred by the echo. e.g. if the daughter plate should contain 250 nL of 10 mM DMSO stock in well A1, the concentration for well A1 should be 10. Note that the concentration for the daughter plate refers strictly to the solution transferred by the echo; it does not refer to the final desired concentration for the experiment, which is typically reached by diluting the daughter plate in buffer. If the concentration of a compound in the daughter is the same as the concentration of that compound in the mother, the solution will be directly transferred from the mother to the daugther without further dilution. If the concentration of a compound in the daughter is lower than in the mother, the dilution will be performed by transferring the appropriate mixture of compound and dilutant (typically DMSO) from the mother plate. If the concentration in the daughter is higher than the concentration in the mother, this will result in an error message when the echo instructions are written (this mistake won't cause an error in this step.)
#' @param .volume_col Needed for daughter plates only. The name of the column in the layout containing the final transferred volume in a given well, in nL.
#' @param daughter_names Needed for daughter plates only. A vector containing the final names for the columns referring to in the daughter plate. These should match what is expected by the Echo Plate Reformat Software. Defaults to c("Destination Well", "compound", "daughter_conc", "daughter_final_vol").
#' @param mother_names Needed for mother plates only. A vector containing the final names for the columns referring to the mother plate. These should match what is expected by the Echo Plate Reformat Software. Defaults to c("Source Well", "compound", "mother_conc").
#'
#' @return Returns a layout, standardized for use in downstream echor functions.
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom purrr set_names
#' @importFrom dplyr filter select across
#' @importFrom tidyselect all_of everything
#'
#' @export
#'
standardize_layout <- function(layout,
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
  numeric_cols <- switch(which_plate,
                        "daughter" = c({{ .concentration_col }}, {{ .volume_col }}),
                        "mother" = c({{ .compound_col }}))

  # ensure all requested columns are in the layout, or return error
  assertthat::assert_that(length(col_names) == length(which_cols))

  # ensure length of new name vector matches selected columns, or return error
  assertthat::assert_that(all(as.character(which_cols) %in% names(layout)))

  # condition the layout
  layout %>%
    filter(is.na({{ .compound_col }}) == FALSE) %>%
    select(all_of(which_cols)) %>%
    mutate(across(cols = everything(), as.character),
           across(any_of(c("concentration", "volume")), as.numeric)) %>%
    set_names(col_names)

}
