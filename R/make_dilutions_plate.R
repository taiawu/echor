#' Calculate dilution transfers
#'
#' @param transfers a tibble containging the transfer steps, as calculated by get_transfer_steps()
#' @param standardized_mother The mother plate layout, standardized for echo, as done by standardize_layout(which_plate = "mother")
#' @param .echo_drop_nL The droplet volume for the echo to be used, in nL. Defaults to 25.
#' @param .dil_name The name, as listed in the mother plate, of the substance to be used for dilutions. Defaults to "DMSO"
#'
#' @return all transfers needed to dilute compounds to the requested volume in the daughter plate.
#'
#' @importFrom  magrittr "%>%"
#' @importFrom dplyr filter mutate select distinct
#' @export
#'
make_dilutions_plate <- function(transfers, standardized_mother, .echo_drop_nL = 25, .dil_name = "DMSO") {

  # create a mother for just the dilutions
  dil_mother <- standardized_mother %>%
    filter(.data$compound == .dil_name) %>%
    mutate(mother_conc = 1)

  # create a daughter for just the dilutions
  dil_daughter <- transfers %>%
    filter(.data$dilution_vol > 0) %>%
    mutate(daughter_final_vol = .data$dilution_vol) %>%
    select(.data$`Destination Well`, .data$daughter_final_vol, ) %>%
    mutate(compound = .dil_name,
           daughter_conc = 1) %>%
    distinct() # there are duplicates for each transfer in the daughter!

  dil_transfer <- calculate_dilutions(dil_daughter, dil_mother, .echo_drop_nL) %>%
    distribute_across_source(.echo_drop_nL)

}
