#### repair any concentrations in which the daughter concentration is higher than the mother
#'
#' Repair daughter plates that request impossible concentrations
#'
#' If the concentration of a compound in the daughter plate is higher than its concentration in the mother plate, the transfer cannot be completed. This function catches and handles that scenario.
#'
#' @param transfers a tibble containging the transfer steps, as calculated by get_transfer_steps()
#' @param conc_error action to take to resolve the request. "stop" will throw an error, and halt execution. "drop" removes these wells from the daughter palte. "scale_down" reduces all concentrations of that compound in the daugther by an equal factor, such that the tested concentrations remain the same relative to one another, but are all at or below the concentration present in the mother. If "make_max", overwrites the concentration of that compound in the daughter to the concentration in the mother in only the wells in which the requested daughter concentration is higher than the concentration of that compound in the mother.
#' @param .echo_drop_nL the tarnsfer drop volume for the echo to be used. Defaults to 25 nL.
#'
#' @return the transfers tibble, with all concentrations in the daughter at or below the concentration in the mother.
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull filter group_by mutate ungroup bind_rows
#' @export
#'

repair_conc_error <- function(transfers, conc_error = "stop", .echo_drop_nL = 25) {
  ## -- daughter concentrations higher than mother concentration
  if (any( transfers$mother_vol > transfers$daughter_final_vol )) {
    # bool selector for problem transfers
    error_rows <- transfers$mother_vol > transfers$daughter_final_vol

    # option 1: drop the problem wells from the daughter
    if (conc_error == "stop") {

      assertthat::assert_that(any( transfers$mother_vol > transfers$daughter_final_vol ) == FALSE ) # throw an error

     } else if (conc_error == "drop") {
      transfers <- transfers[!error_rows,]

      # option 2: scale down all occurences of this compound in the daughter to those achievable by mother
    } else if (conc_error == "scale_down") {
      error_cmpds <- transfers[error_rows,] %>% pull(.data$compound) %>% unique() # identify which compounds have errors

      # scale down the concentration in the daughter equally wherever this compound appears
      dil_daughter <-  transfers %>%
        filter(.data$compound %in% error_cmpds) %>%
        group_by(.data$compound) %>%
        mutate(daughter_conc = .data$daughter_conc*(.data$mother_conc/max(.data$daughter_conc))) %>%
        ungroup() %>%
        calculate_dilutions(., .echo_drop_nL)

      # re-append this to the rest of the transfers
      transfers <- transfers %>%
        filter(!.data$compound %in% error_cmpds) %>%
        bind_rows(dil_daughter)

      # option 3: replace over-concentrated wells with undiluted mother stock
    } else if (conc_error == "make_max" ) {
      # where ever the daughter concentration exceeds the mother, just make it the mother concentration
      transfers <- transfers %>%
        mutate(daughter_conc =  replace(.data$daughter_conc, error_rows, .data$mother_conc[error_rows])) %>%
        calculate_dilutions(., .echo_drop_nL)

      # option 4 (default): throw an error
    } else {
      assertthat::assert_that(any(transfers$daughter_final_vol - transfers$mother_vol < 0) == FALSE )
    }
  }

  transfers
}
