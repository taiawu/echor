#' Write echo instructions
#'
#' Given the transfer steps calculated with get_transfer_steps(), write the full echo instructions file. This includes adding the dilution steps, standardizing column names, and adding additional columns used by the Echo Reformat plate software. Includes a check that none of the wells in the mother used for dilutions will be used to transfer a greater volume than they contain. This volume, given in nL, defaults to 40000 -- equivalent to 40 uL.
#'
#' @param transfers A tibble containing the calculated transfer steps, as created by get_transfer_steps()
#' @param standardized_mother The mother plate layout, standardized for echo, as done by standardize_layout(which_plate = "mother")
#' @param .echo_drop_nL The droplet volume for the echo to be used, in nL. Defaults to 25.
#' @param .dil_name The name, as listed in the mother plate, of the substance to be used for dilutions. Defaults to "DMSO"
#' @param .max_pull_nL The maximum volume (in nL) that can be pulled from any one well in the mother plate for dilutions.
#'
#' @return A tibble which can be saved as a csv and uploaded directly to the Echo Plate Reformat software.
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom dplyr ungroup bind_rows rename mutate select
#' @importFrom tidyselect all_of
#'
#' @export
#'
write_echo_instructions <- function(transfers, standardized_mother, .echo_drop_nL = 25, .dil_name = "DMSO", .max_pull_nL = 40000) {

  ## ensure the mother has at least one well of the dilution material
  assertthat::assert_that(.dil_name %in% standardized_mother$compound)

  # ## calculate the dilution transfers
  dil_plate <- make_dilutions_plate(transfers, standardized_mother, .echo_drop_nL = 25, .dil_name = "DMSO")


  # calculate the final instructions
  transfers %>%
    ungroup() %>%
    bind_rows(dil_plate) %>%
    rename("Transfer Volume" = .data$mother_vol) %>%

    mutate("Source Plate Name" = "Source[1]",
           "Destination Plate Name" = "Destination[1]",
           "Destination Well X Offset"	= NA,
           "Destination Well Y Offset"	= NA) %>%

    select(all_of(c("Source Plate Name",	"Source Well",
                    "Destination Plate Name",	"Destination Well",
                    "Transfer Volume",	"Destination Well X Offset",
                    "Destination Well Y Offset")))
}
