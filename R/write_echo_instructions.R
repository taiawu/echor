#' Write echo instructions
#'
#' Given the transfer steps calculated with get_transfer_steps(), write the full echo instructions file. This includes adding the dilution steps, standardizing column names, and adding additional columns used by the Echo Reformat plate software. Includes a check that none of the wells in the mother used for dilutions will be used to transfer a greater volume than they contain. This volume, given in nL, defaults to 40000 -- equivalent to 40 uL.
#'
#' @param trans_data A tibble containing the calculated transfer steps, as created by get_transfer_steps()
#' @param standardized_mother The conditions
#' @param .dil_name The name of the compound in the mother used for dilutions. Defaults to "DMSO".
#' @param .max_pull_nL The maximum volume (in nL) that can be pulled from any one well in the mother plate for dilutions.
#'
#' @return A tibble which can be saved as a csv and uploaded directly to the Echo Plate Reformat software.
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter select mutate left_join rename group_by summarise pull bind_rows select
#' @importFrom tidyselect all_of
#'
#' @export
#'
write_echo_instructions <- function(trans_data, standardized_mother, .dil_name = "DMSO", .max_pull_nL = 40000) {

  ## ensure the mother has at least one well of the dilution material
  assertthat::assert_that(.dil_name %in% standardized_mother$compound)

  dil_df <- trans_data %>%
    filter(.data$dilution_vol > 0) %>%
    select(.data$`Destination Well`, .data$`dilution_vol`) %>%
    mutate(compound = .dil_name) %>%
    left_join(standardized_mother, by = "compound") %>%
    rename("Transfer Volume" = .data$dilution_vol) %>%
    select(.data$`Destination Well`, .data$`Source Well`, .data$`Transfer Volume`)

  ## ensure the mother has at least one well of the dilution material
  dil_vol_total <- dil_df %>%
    group_by(.data$`Source Well`) %>%
    summarise(total_transfer_vol = sum(.data$`Transfer Volume`)) %>%
    pull(.data$total_transfer_vol)

  ## throw an error if the maximum volume is exceeded
  assertthat::assert_that(all(.max_pull_nL > dil_vol_total))

  # calculate the final instructions
  trans_data %>%
    rename("Transfer Volume" = .data$mother_vol) %>%
    bind_rows(dil_df) %>%

    mutate("Source Plate Name" = "Source[1]",
           "Destination Plate Name" = "Destination[1]",
           "Destination Well X Offset"	= NA,
           "Destination Well Y Offset"	= NA) %>%

    select(all_of(c("Source Plate Name",	"Source Well",
                    "Destination Plate Name",	"Destination Well",
                    "Transfer Volume",	"Destination Well X Offset",
                    "Destination Well Y Offset")))
}
