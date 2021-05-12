### handle multiple mother concentrations
#' Handle varied compound concentrations in the mother plate
#'
#' This version of the code requires that each compound in the mother occurs at only one concentration. This function checks if any compounds are in the mother plateat muliple concentartions, and resolves this by either halting execution in an error (used as a stopping point for user-defined action), using only the well(s) with the highest concentration, or using only the wells which contain the most represented concentration.
#'
#' @param mother mother plate layout, standardized for echo functions with standardize_layout()
#' @param keep_which how to handle compounds that appear in the mother at multiple concentrations."stop" will produce an error, halting execution. "max_conc" will keep only the well(s) with the highest concentration. "most_wells" will keep the concentration in the greatest number of wells, removing all wells which do not have that compound at that concentration.
#'
#'
#' @return the mother layout, with all compounds occuring in only a single concentration. The wells containing that compound at a different concentration have been removed.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by mutate n_distinct ungroup filter select
#' @export

handle_varied_mother_concentrations <- function(mother, keep_which = "max_conc") {

  tallied <- mother %>%
    group_by(.data$compound) %>% # for each compound
    mutate(n_conc = n_distinct(.data$mother_conc), # how many conc. in mother
           max_conc = max(.data$mother_conc)) %>% # which conc is highest
    group_by(.data$compound, .data$mother_conc) %>%
    mutate(well_per_conc = n_distinct(.data$`Source Well`), # how many wells per conc
           most_wells = max(.data$well_per_conc)[[1]]) %>% # which conc has the most wells
    ungroup()

  out <- switch(keep_which,
                "max_conc" = tallied %>% filter(.data$mother_conc == .data$max_conc), # keep the wells with the highest conc
                "most_wells" = tallied %>% filter(.data$well_per_conc == .data$most_wells)) # keep the concentrations with the most wells

  # remove the columns created by these operations
  out %>%
    select(-c(.data$n_conc, .data$max_conc, .data$well_per_conc, .data$most_wells ))

}
