#' Write the transfer steps for compounds
#'
#' This function handles inconsistencies and errors in the daughter layout. The dilution transfer steps and finalization of formatting for reading by the echo are handled by write_echo_instructions()
#'
#' @param standardized_daugther The daughter plate layout, standardized for echo, as done by standardize_layout(which_plate = "daughter")
#' @param standarized_mother The mother plate layout, standardized for echo, as done by standardize_layout(which_plate = "mother")
#' @param .echo_drop_nL The droplet volume for the echo to be used, in nL. Defaults to 25.
#' @param missing_cmpds How to handle compounds present in the daughter plate, but not the mother. Options are "stop", which halts execution and errors, and "drop", which removes all of these wells from the daughter layout. Defaults to "stop".
#' @param conc_error How to handle compounds present in the daughter plate at higher concentration than in the mother (impossible to achieve). Options are "stop", which halts execution and errors, and "drop", which removes all of these wells from the daughter layout, "scale_down", which takes all occurances of that compound in the daughter plate and reduces their concentration such that relative dilutions are unchanged, but scaled down so that the highest concentration is the same as the concentration in the mother plate, and "make_max", which replaces these concentrations with the maximum achievable concentration, which is the concentration in the mother plate. Defaults to "stop".
#'
#' @return A tibble containing all transfer steps for all compounds in the daughter plate. Transer steps for dilutions are appended in write_echo_instructions().
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate ungroup
#' @importFrom utils "globalVariables"
#'
#' @export
#'
### get the transfer steps
get_transfer_steps <- function(standardized_daugther,
                               standarized_mother,
                               .echo_drop_nL = 25,
                               missing_cmpds = "drop",
                               conc_error = "drop") {
  ## in a shiny app, the workflow would probably be: throw an error catcher, and user can select what to do going forward, recalling with new values for missing_cmpds and conc_error


  ####### ---- ensure valid options selected for transfer issues
  assertthat::assert_that(missing_cmpds %in% c("stop", "drop"))
  assertthat::assert_that(conc_error %in% c("stop", "drop", "scale_down", "make_max"))

  ####### ---- calculate the transfers
  cmpd_trans <- calculate_dilutions(standardized_daugther, standarized_mother, .echo_drop_nL) %>%
    repair_conc_error(conc_error, .echo_drop_nL) %>%
    mutate(dilution_vol = .data$daughter_final_vol - .data$mother_vol) %>%
    distribute_across_source(.echo_drop_nL) %>%
    ungroup()

}

utils::globalVariables(c("."))
