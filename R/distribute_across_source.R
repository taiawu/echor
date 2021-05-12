#### distribute the transfers across wells
#' Distribute transfers across all source wells of a given compound
#'
#' @param transfers a tibble containging the transfer steps, as calculated by get_transfer_steps()
#' @param .echo_drop_nL the tarnsfer drop volume for the echo to be used. Defaults to 25 nL.
#'
#' @return the transfers tibble, with transfers now distributed across all source wells containing that compound.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate rename select row_number
#' @importFrom purrr map_dbl
#' @importFrom tidyr unnest
#'
#' @export
#'

distribute_across_source <- function(transfers, .echo_drop_nL = 25) {

  # divide the transfer up equally among the mother wells
  transfers %>%
    mutate(n_wells = map_dbl(.data$data, nrow)) %>% #  the number of wells of mother
    mutate(per_well = (.data$mother_vol/.data$n_wells) - (.data$mother_vol/.data$n_wells)%%.echo_drop_nL, # how many transfers per well?
           extra_transfer = .data$mother_vol - .data$per_well*.data$n_wells) %>% # how many left over transfers, after even division over wells?
    rename(mother_vol_total = .data$mother_vol,
           mother_vol = .data$per_well) %>%
    unnest(cols = c(.data$data)) %>%  # this unnesting step adds the divided transfer volume to all mother source wells
    group_by(.data$compound, .data$mother_conc, .data$`Destination Well`, .add = TRUE) %>%
    mutate(mother_vol = if_else(row_number() == 1, .data$mother_vol + .data$extra_transfer, .data$mother_vol)) %>% # add the extra transfer to just one of the wells
    select(-.data$extra_transfer) # drop this extra, unneeded column

}
