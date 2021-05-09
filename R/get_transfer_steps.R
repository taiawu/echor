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
#' @importFrom dplyr left_join filter mutate group_by ungroup bind_rows pull
#' @importFrom plyr round_any
#' @importFrom rlang .data
#' @importFrom utils "globalVariables"
#'
#' @export
#'
get_transfer_steps <- function(standardized_daugther,
                               standarized_mother,
                               .echo_drop_nL = 25,
                               missing_cmpds = "stop",
                               conc_error = "stop") {
  ## in a shiny app, the workflow would probably be: throw an error catcher, and user can select what to do going forward, recalling with new values for missing_cmpds and conc_error

  ####### ---- ensure valid options selected for transfer issues
  assertthat::assert_that(missing_cmpds %in% c("stop", "drop"))
  assertthat::assert_that(conc_error %in% c("stop", "drop", "scale_down", "make_max"))


  ####### ---- calculate the transfers
  joined <- left_join(standardized_daugther, standarized_mother, by = "compound")
  cmpd_trans <- calculate_dilutions(joined, .echo_drop_nL)

  ####### ---- handle any issues with the daughter layout
  ## -- if there are compounds present the daughter but not the mother
  if (all(standardized_daugther$compound %in% standarized_mother$compound) == FALSE) {
    # option 1: drop the problem wells from the daughter plate
    if (missing_cmpds == "drop") { # drop the missing compounds from the daughter
      cmpd_trans <- cmpd_trans %>% filter(is.na(.data$mother_vol) == FALSE)

      # option 2 (default): throw an error
    } else { # or throw an error
      assertthat::assert_that(all(standardized_daugther$compound %in% standarized_mother$compound))
    }
  }

  ## -- daughter concentrations higher than mother concentration
  if (any( cmpd_trans$mother_vol > cmpd_trans$daughter_final_vol )) {
    # bool selector for problem rows
    error_rows <- cmpd_trans$mother_vol > cmpd_trans$daughter_final_vol

    # option 1: drop the problem wells from the daughter
    if (conc_error == "drop") {
      cmpd_trans <- cmpd_trans[!error_rows,]

      # option 2: scale down all occurences of this compound in the daughter to those achievable by mother
    } else if (conc_error == "scale_down") {
      error_cmpds <- cmpd_trans[error_rows,] %>% pull(.data$compound) %>% unique() # identify which compounds have errors

      # scale down the concentration in the daughter equally wherever this compound appears
      dil_daughter <-  cmpd_trans %>%
        filter(.data$compound %in% error_cmpds) %>%
        group_by(.data$compound) %>%
        mutate(daughter_conc = .data$daughter_conc*(.data$mother_conc/max(.data$daughter_conc))) %>%
        ungroup() %>%
        calculate_dilutions(., .echo_drop_nL)

      # re-append this to the rest of the transfers
      cmpd_trans <- cmpd_trans %>%
        filter(!.data$compound %in% error_cmpds) %>%
        bind_rows(dil_daughter)

      # option 3: replace over-concentrated wells with undiluted mother stock
    } else if (conc_error == "make_max" ) {
      # where ever the daughter concentration exceeds the mother, just make it the mother concentration
      cmpd_trans <- cmpd_trans %>%
        mutate(daughter_conc =  replace(.data$daughter_conc, error_rows, .data$mother_conc[error_rows])) %>%
        calculate_dilutions(., .echo_drop_nL)

      # option 4 (default): throw an error
    } else {
      assertthat::assert_that(any(cmpd_trans$daughter_final_vol - cmpd_trans$mother_vol < 0) == FALSE )
    }
  }

  cmpd_trans %>%
    mutate(dilution_vol = .data$daughter_final_vol - .data$mother_vol)

}

utils::globalVariables(c("."))
