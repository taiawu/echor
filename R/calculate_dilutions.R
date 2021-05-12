#' Calculate dilutions between mother and daughter plates
#'
#' A helper funciton used inside of calculate_transfers() to calculate the volume of compound to transfer from mother to daughter wells.
#'
#' @param daughter daughter plate layout, standardized for echo functions with standardize_layout()
#' @param mother mother plate layout, standardized for echo functions with standardize_layout()
#' @param .echo_drop_nL the droplet size of the Echo to be used, in nL. Defaults to 25.
#'
#' @return the input tibble, with additional columns describing the volume to be transferred from the mother, and the extent to which rounding the input concentration was necessary, given the droplet size of the echo used.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by left_join mutate if_else
#' @importFrom tidyr nest
#' @importFrom plyr round_any
#' @importFrom rlang .data
#'
#'
#' @export
#'
#'
#
calculate_dilutions <- function(daughter, mother, .echo_drop_nL = 25) { # if there are compounds in the daughter not in the mother

by_compound_m <- mother %>%
  group_by(.data$compound, .data$mother_conc) %>%
  nest()

daughter %>%
  left_join( . , by_compound_m, by = "compound")  %>%
  mutate(mother_dil = (.data$daughter_conc/.data$mother_conc) * ( .data$daughter_final_vol),
         mother_vol = plyr::round_any(.data$mother_dil, .echo_drop_nL, ceiling),
         final_conc = (.data$mother_conc*.data$mother_vol)/.data$daughter_final_vol,
         rounded_up = .data$final_conc - .data$daughter_conc,
         rounded_up_perc = if_else(.data$daughter_conc == 0, true = 0, false = round(100*.data$rounded_up/.data$daughter_conc, 1)))
}
#
#
# calculate_dilutions <- function(cmpd_joined, .echo_drop_nL = 25) { # if there are compounds in the daughter not in the mother
#   # transfers will evenly distribute across identical mother wells because of left_join default behavior
#   cmpd_joined %>%
#     mutate(mother_dil = (.data$daughter_conc/.data$mother_conc) * ( .data$daughter_final_vol),
#            mother_vol = plyr::round_any(.data$mother_dil, .echo_drop_nL, ceiling),
#            final_conc = (.data$mother_conc*.data$mother_vol)/.data$daughter_final_vol,
#            rounded_up = .data$final_conc - .data$daughter_conc,
#            rounded_up_perc = if_else(.data$daughter_conc == 0, true = 0, false = round(100*.data$rounded_up/.data$daughter_conc, 1)))
# }
