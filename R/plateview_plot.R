#' Make a Plate-view plot
#'
#' Makes a plot that mimics looking down at a well plate. Wells are colored based on a user-defined column from a layout.
#'
#' @param layout_data a layout tibble (as created by dsfworld::read_plate_layout()), containing columns for plate row (called "row"), plate column (called "column"), and one variable by which the plate will be colored
#' @param .fill_var the column in the input layout by which to color in the wells in the plot
#' @param .well_col the name of the column containing the wells. Defaults to "well".
#' @param col_breaks the number of tick marks and lines in the x axis (columns). Defaults to every 6 columns.
#' @param .title title for the plot. Defaults to "Plate-view plot"
#' @param .scale_fill the color scale to use in the plot. Defaults to scale_color_viridis_d(). Must be updated when plotting continuous variables (e.g. to scale_color_viridis_c())
#'
#' @return a plate-view plot, with wells colored based on the information on that well in the user-defined variable given in .fill_var.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter rename
#' @importFrom ggplot2 ggplot aes scale_y_discrete geom_point scale_x_continuous theme labs scale_fill_viridis_d element_rect element_blank element_line
#' @importFrom plyr rbind.fill
#' @importFrom tidyselect any_of
#' @importFrom tidyr fill
#' @importFrom stringr str_extract_all str_to_upper
#' @importFrom purrr as_vector
#' @importFrom readr parse_number
#'
#' @export
#'
plateview_plot <- function(layout_data,
                       .fill_var,
                       .well_col = "well",
                       col_breaks = 6,
                       .title = "Plate-view plot",
                       .scale_fill = scale_fill_viridis_d()) {

  layout_data <- layout_data %>%
                      rename(well = .well_col) %>%
                        add_empty_wells( . )

  plate_data <- layout_data %>%
    filter(is.na( {{ .fill_var }}) == FALSE)

  blank_plate <- layout_data %>%
    ggplot(aes(x = .data$column, y = .data$row)) +
    scale_y_discrete(limits = rev) +
    geom_point(color ="#737373",
               shape = 22,
               size = 4,
               alpha = 1)  +
    scale_x_continuous(breaks = seq(from = 1, to = 24, by = col_breaks)) +
    theme(aspect.ratio = 16/24,
          panel.background  = element_rect(color = "#525252", fill = "#525252"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#737373", size = 0.5),
          legend.position = "right",
          axis.title = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
    )

  plate <- blank_plate +
    geom_point(data = plate_data,
               aes(fill = {{ .fill_var }}),
               color = "#969696",
               shape = 22,
               size = 4,
               alpha = 1)  +
    .scale_fill +
    labs(title = .title)

}
