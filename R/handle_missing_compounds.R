### handle compounds missing from the mother asked for in the daughter
#' Resolve cases when a compound is not present in the mother
#'
#' Allow users to choose how to resolve cases when a compound in the daughter plate is not present in the mother plate, and therefore cannot be transferred. Compounds are mapped between the daughter and mother plates by name, so compounds must have exactly the the same name in the mother and daughter plates for the trasnfer instructions to be written (case sensitive).
#'
#' @param mother mother plate layout, standardized for echo functions with standardize_layout()
#' @param daughter daughter plate layout, standardized for echo functions with standardize_layout()
#' @param missing_cmpds how to handle missing compounds. "stop" will produce an error, halting execution. "drop" will remove the missing compounds from the daughter layout.
#'
#' @return if there are no missing compounds, returns the daughter layout unchanged. If there are missing compounds, returns the daughterlayout with the missing compounds removed if missing_cmps is set to "drop", or an error, if missing_cmods is set to "stop"
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter
#'
#' @export
#'
handle_missing_compounds <-function(mother, daughter, missing_cmpds = "stop") {

  assertthat::assert_that(missing_cmpds %in% c("stop", "drop"))

  daughter_cmpds <- daughter$compound
  mother_cmpds <- mother$compound

  if (all(daughter_cmpds %in% mother_cmpds) == FALSE) {
    missing <- daughter_cmpds[!daughter_cmpds %in% mother_cmpds]

    if (missing_cmpds == "drop") { # drop the missing compounds from the daughter
      daughter <- daughter %>% filter(.data$compound %in% mother_cmpds)

      # option 2 (default): throw an error
    } else { # or throw an error
      assertthat::assert_that(all(daughter_cmpds %in% mother_cmpds))
    }
  }
  daughter
}
