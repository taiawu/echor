###### ------- Functions to make the echo instructions

echo_round <- function( x, .round = 25 ) {
  if ( x%%.round == 0 ) {
    return( x )
  } else {
    x + .round - x%%.round 
  }
}

add_DMSO_dil <- function(df_all, df_mother) {
  daughter_DMSO <- df_all %>% 
    filter(.data$DMSO_vol > 0 )
  
  mother_DMSO <- df_mother %>% 
    filter(.data$compound == "DMSO")
  
  n_DMSO_daughter <- daughter_DMSO %>% nrow() # number of DMSO-diluted wells in daugther
  n_DMSO_mother <- mother_DMSO %>% nrow()
  
  mother_reps <- ceiling(n_DMSO_daughter/n_DMSO_mother) 
  
  merge_vec <- rep(mother_DMSO$`Source Well`, times = mother_reps) %>%
    .[c(1:n_DMSO_daughter)]
  
  daughter_DMSO_out <- daughter_DMSO %>%
    mutate("Source Well" = merge_vec) %>%
    mutate(mother_vol = DMSO_vol)
}

###### ------- Functions to add empty wells to partial-plate layouts.
## useful for plotting plates, and conditioning for echo
add_empty_wells <- function(df, 
                            n_wells = "384",
                            .df_well_col = well,
                            .fill_down_cols = "", 
                            add_rows_cols = TRUE) {
  well_vec <- switch(n_wells,
                     "384" = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","L1","M1","N1","O1","P1","A2","B2","C2","D2","E2","F2","G2","H2","I2","J2","K2","L2","M2","N2","O2","P2","A3","B3","C3","D3","E3","F3","G3","H3","I3","J3","K3","L3","M3","N3","O3","P3","A4","B4","C4","D4","E4","F4","G4","H4","I4","J4","K4","L4","M4","N4","O4","P4","A5","B5","C5","D5","E5","F5","G5","H5","I5","J5","K5","L5","M5","N5","O5","P5","A6","B6","C6","D6","E6","F6","G6","H6","I6","J6","K6","L6","M6","N6","O6","P6","A7","B7","C7","D7","E7","F7","G7","H7","I7","J7","K7","L7","M7","N7","O7","P7","A8","B8","C8","D8","E8","F8","G8","H8","I8","J8","K8","L8","M8","N8","O8","P8","A9","B9","C9","D9","E9","F9","G9","H9","I9","J9","K9","L9","M9","N9","O9","P9","A10","B10","C10","D10","E10","F10","G10","H10","I10","J10","K10","L10","M10","N10","O10","P10","A11","B11","C11","D11","E11","F11","G11","H11","I11","J11","K11","L11","M11","N11","O11","P11","A12","B12","C12","D12","E12","F12","G12","H12","I12","J12","K12","L12","M12","N12","O12","P12","A13","B13","C13","D13","E13","F13","G13","H13","I13","J13","K13","L13","M13","N13","O13","P13","A14","B14","C14","D14","E14","F14","G14","H14","I14","J14","K14","L14","M14","N14","O14","P14","A15","B15","C15","D15","E15","F15","G15","H15","I15","J15","K15","L15","M15","N15","O15","P15","A16","B16","C16","D16","E16","F16","G16","H16","I16","J16","K16","L16","M16","N16","O16","P16","A17","B17","C17","D17","E17","F17","G17","H17","I17","J17","K17","L17","M17","N17","O17","P17","A18","B18","C18","D18","E18","F18","G18","H18","I18","J18","K18","L18","M18","N18","O18","P18","A19","B19","C19","D19","E19","F19","G19","H19","I19","J19","K19","L19","M19","N19","O19","P19","A20","B20","C20","D20","E20","F20","G20","H20","I20","J20","K20","L20","M20","N20","O20","P20","A21","B21","C21","D21","E21","F21","G21","H21","I21","J21","K21","L21","M21","N21","O21","P21","A22","B22","C22","D22","E22","F22","G22","H22","I22","J22","K22","L22","M22","N22","O22","P22","A23","B23","C23","D23","E23","F23","G23","H23","I23","J23","K23","L23","M23","N23","O23","P23","A24","B24","C24","D24","E24","F24","G24","H24","I24","J24","K24","L24","M24","N24","O24","P24"),
                     "96" = c("A1","B1","C1","D1","E1","F1","G1","H1","A2","B2","C2","D2","E2","F2","G2","H2","A3","B3","C3","D3","E3","F3","G3","H3","A4","B4","C4","D4","E4","F4","G4","H4","A5","B5","C5","D5","E5","F5","G5","H5","A6","B6","C6","D6","E6","F6","G6","H6","A7","B7","C7","D7","E7","F7","G7","H7","A8","B8","C8","D8","E8","F8","G8","H8","A9","B9","C9","D9","E9","F9","G9","H9","A10","B10","C10","D10","E10","F10","G10","H10","A11","B11","C11","D11","E11","F11","G11","H11","A12","B12","C12","D12","E12","F12","G12","H12"))
  
  
  df_wells <- df %>% pull({{ .df_well_col }}) %>% unique()
  
  df_out <- tibble(well = well_vec %>% as.character()) %>%
    filter(!.data$well %in% df_wells) %>%
    plyr::rbind.fill(df,  . ) %>%# fills in everything 
    as_tibble() %>% # convert back to tibble from data.frame
    fill(any_of(.fill_down_cols), .direction = "down")
  
  if (add_rows_cols == TRUE) {
    
    df_out <- df_out %>%
      mutate(row =  str_extract_all(.data$well, "[A-Z; a-z]", simplify = TRUE) %>%
               str_to_upper(locale = "en") %>%
               as_vector(),
             column = parse_number(.data$well))
  }
  
  df_out
  
}

###### ------- Functions to apply wells to conditions 
conditions_to_layout <- function(df, 
                                 well_df, 
                                 .well_order = by_col,
                                 .fill_down_cols = "plate") {
  
  # takes conditions and maps into layout, filling NA for all empties
  wells <- well_df %>% 
    select( {{ .well_order }}, .data$well_num) %>%
    set_names(c("well", "well_num")) %>%
    mutate(well =  .data$well %>% as.character())
  
  df_out <- df %>% left_join( . , wells, by = "well_num")
  
  # fill in the rest of the plate with NA?
  df_out <-  df_out %>%
    add_empty_wells( . ,
                     .fill_down_cols = .fill_down_cols,
                     add_rows_cols = TRUE
    )
}

condition_layout_for_echo <- function(layout, 
                                      which_plate,
                                      .well_col = "well",
                                      .compound_col = "compound",
                                      .concentration_col = "concentration",
                                      .volume_col = "volume"
) {
  
  
  if (which_plate == "daughter") {
    out <- layout %>% 
      filter(is.na({{ .compound_col }}) == FALSE) %>%
      select({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}, {{ .volume_col }}) %>% 
      set_names(c("well", "compound", "concentration", "volume")) %>%
      mutate(across(cols = c("concentration", "volume"), as.numeric()))  %>%
      rename("Destination Well" = "well",
             "daughter_conc" = "concentration",
             "daughter_final_vol" = "volume") %>%
      filter(.data$daughter_final_vol != 0)
  } else if (which_plate == "mother") {
    out <- layout %>% 
      filter(is.na({{ .compound_col }}) == FALSE) %>%
      select({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}) %>% 
      set_names("well", "compound", "concentration") %>%
      rename("Source Well" = "well",
             "mother_conc" = "concentration")
  }
  
  out
}


make_echo_instructions <- function(conditioned_daughter, 
                                   conditioned_mother, 
                                   .echo_drop_nL = 25,
                                   .return_value = "echo_instructions") {
  #### combine mother and daughter layouts
  all_values <- conditioned_daughter %>%
    left_join(conditioned_mother, by = "compound") %>%
    mutate(mother_dil = (.data$daughter_conc/.data$mother_conc) * ( .data$daughter_final_vol)) %>%
    mutate(mother_vol = sapply(X =.data$mother_dil, FUN = echo_round, .round = .echo_drop_nL), #### THIS IS WHERE THE ROUNDING HAPPENS
           DMSO_vol = .data$daughter_final_vol - .data$mother_vol, 
           "Source Plate Name" = rep("Source[1]", times = nrow(.)),
           "Destination Plate Name" = rep("Destination[1]", times = nrow(.)),
           "Destination Well X Offset"	= rep(NA, times = nrow(.)),
           "Destination Well Y Offset"	= rep(NA, times = nrow(.)))
  
  # add DMSO dilution volumes
  final_values <- add_DMSO_dil(all_values, conditioned_mother) %>%
    bind_rows(all_values, . ) %>%
    mutate(final_daughter_conc = (.data$mother_vol * .data$mother_conc)/.data$daughter_final_vol) 
  
  echo_instructions <- final_values %>%
    rename("Transfer Volume" = .data$mother_vol) %>%
    select(all_of(c("Source Plate Name",	"Source Well",
                    "Destination Plate Name",	"Destination Well",
                    "Transfer Volume",	"Destination Well X Offset",
                    "Destination Well Y Offset")))
  
  out <- switch(.return_value,
                "echo_instructions" = echo_instructions,
                "final_values"=  final_values
                )
  
  out
 
}


##### Automated file creation and saving 
layout_to_echo <- function(daughter_layout, mother_layout) {
  
  mother_layout <-  mother_layout %>% condition_layout_for_echo(which_plate = "mother")
  
  daughter_layout %>% 
    mutate(conditioned_for_echo = map(data, condition_layout_for_echo, which_plate = "daughter", 
                                      .well_col = "well",
                                      .compound_col = "cmpd",
                                      .concentration_col = "final_test_conc",
                                      .volume_col = "echo_vol_nL")) %>%
    
    mutate(echo_final_cond = map(conditioned_for_echo, make_echo_instructions, mother_layout, .echo_drop_nL = 2.5, .return_value = "final_values"), 
           echo_instructions = map(conditioned_for_echo, make_echo_instructions, mother_layout, .echo_drop_nL = 2.5,  .return_value = "echo_instructions"))
  
}

plate_plot <- function(layout_data, 
                       .color_var, 
                       .fill_var, 
                       col_breaks = 6, 
                       .title = "",
                       .save_plot = TRUE,
                       .save_path,
                       .save_name,
                       .save_width = 7,
                       .save_height = 5,
                       .scale_color = scale_color_viridis_d(),
                       .scale_fill = scale_fill_viridis_d()) {
  
  plate_data <- layout_data %>%
    filter(is.na( {{ .fill_var }}) == FALSE) %>%
    filter(is.na( {{ .color_var }}) == FALSE)
  
  blank_plate <- layout_data %>%
    ggplot(aes(x = column, y = row)) +
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
    
    .scale_color +
    .scale_fill +
    labs(title = .title)
  
  if (.save_plot == TRUE ) {
    ggsave(glue::glue(.save_path, .save_name, ".pdf"), plate, width = .save_width, height = .save_height, bg = "transparent")
  } 
  
  out <- plate
  
}

save_echo_instructions <- function(daughter_layout, mother_layout,
                                   .title,
                                   .save_name,
                                   .save_path,
                                   .save_width = 11,
                                   .save_height = 10) {
  # make the directories
  supp_path <- glue::glue(.save_path, "/supporting_files/")
  dir.create(.save_path)
  dir.create(supp_path)
  
  # make the echo instructions
  echo_instr <- layout_to_echo(daughter_layout, mother_layout)
  
  # save the echo instructions
  echo_instr %>% 
    ungroup() %>%
    select(echo_instructions) %>%
    unnest(cols = c(echo_instructions)) %>%
    write_csv(x = . , file = glue::glue(.save_path, .save_name, "echo_instructions.csv" ))
  
  # save the plots
  echo_final_plots(final_data = echo_instr, 
                   .title  = glue::glue(.title),
                   .save_name = glue::glue(.save_name, "plate_plot"),
                   .save_path = glue::glue(.save_path),
                   .save_width = .save_width,
                   .save_height = .save_height)
  
  # save the final values
  echo_instr %>%
    ungroup() %>%
    select(echo_final_cond) %>%
    unnest(cols = c(echo_final_cond)) %>%
    write_csv(x = . , file = glue::glue(supp_path, .save_name, "echo_final_cond.csv" ))
  
  # save the original 
  daughter_layout %>% ungroup() %>% select(data) %>% unnest(cols = c(data)) %>% write_csv(x = . , file = glue::glue(supp_path, .save_name, "daughter_layout.csv" ))
  mother_layout %>% write_csv(x = . , file = glue::glue(supp_path, .save_name, "mother_layout.csv" ))  
}

echo_final_plots <- function(final_data, 
                             .final_cond_col = echo_final_cond,
                             .title,
                             .save_name,
                             .save_path,
                             .save_width = 7, 
                             .save_height = 12) {
  
  plot_data <- final_data %>%
    ungroup() %>%
    select({{ .final_cond_col }}) %>%
    unnest(cols = c({{ .final_cond_col }})) %>%
    rename(well = "Destination Well") %>%
    add_empty_wells( . ) %>%
    mutate(row =  str_extract_all(.data$well, "[A-Z; a-z]", simplify = TRUE) %>%
             str_to_upper(locale = "en") %>%
             as_vector(),
           column = parse_number(.data$well)) 
  
  cmpd_p <- plot_data %>%
    plate_plot(.,
               .color_var = compound, 
               .fill_var = compound,
               col_breaks = 1,
               .title = glue::glue(.title, "_compounds"),
               .save_path = .save_path,
               .save_name = glue::glue(.save_name,"_compounds"),
               .save_plot = FALSE
               # .save_width = .save_width_compounds,
               # .save_height = .save_height_compounds
    )
  
  conc_p <- plot_data %>%
    plate_plot(.,
               .color_var = final_daughter_conc, 
               .fill_var = final_daughter_conc,
               col_breaks = 1,
               .title = glue::glue(.title, "_concentration"),
               .save_path = .save_path,
               .save_name = glue::glue(.save_name, "_concentration"),
               .save_plot = FALSE,
               # .save_width = .save_width_concentrations,
               # .save_height = .save_height_concentrations,
               .scale_color = scale_color_viridis_c(),
               .scale_fill = scale_fill_viridis_c(direction = -1)
    )
  
  figure <- ggpubr::ggarrange(cmpd_p, conc_p, 
                              
                              ncol = 1, nrow = 2,
                              align = "v")
  
  ggsave(glue::glue(.save_path, .save_name, ".pdf"), figure, width = .save_width, height = .save_height, bg = "transparent")
}
