# first time run to ensure fonts are working within R markdown
# install.packages("remotes","extrafont")
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import() # requires confirmation; runs for ~3 minutes
# extrafont::loadfonts(device = 'win')

#### This loads all packages required across scripts (rather than inserting library in each file)
if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}
# downloads and loads libraries
p_load(
  CausalImpact, readr, stringr, dplyr, glue, zoo, ggplot2, tidyr, tidyverse,
  effectsize, png, gridExtra, ggpubr, ggtext, grid, purrr, reshape2,
   curl, paletteer, readxl, scales, extrafont, ggrepel,
  sf, rgeos, maptools, rmarkdown, flextable, officer
)

#### This function sources every file in the src folder
source_files <- function(files_to_source) {
  files_to_source <- list.files("src", pattern = "\\.R$") %>%
    map_chr(~ paste0("src/", .)) %>%
    lapply(source)
}

source_files(files_to_source)

#### Initialises every variable used across scripts (see input_variables.R)
init_vars <- input_variables()

selected_areas_and_labels <- initialise_selected_areas_and_labels(
  init_vars$prop_tests_in_area,
  init_vars$prop_tests_lab_x,
  initialise_areas(init_vars$surrounding_areas),
  init_vars$rest_of_england
  )

# load in the datasets, process some of the data, load map data, load growth rate data
initialised_data <- initialise_raw_data_sets(
  raw_data_file = "cases_admissions_deaths_dataset_publication.csv", 
  lfd_file = "lfd_dataset_publication.csv", 
  pcr_file = "pcr_dataset_publication.csv", 
  surrounding_areas = init_vars$surrounding_areas
)

# create all the visuals present in the preprint
visuals <- finalise_graph_production(
  impact_measure_with_end_date = init_vars$impact_measure_with_end_date, prop_tests_lab_x = init_vars$prop_tests_lab_x,
  rest_of_england = init_vars$rest_of_england, selected_areas = selected_areas_and_labels$selected_areas,
  data = initialised_data$data, apply_log = init_vars$apply_log, revert_log = init_vars$revert_log,
  labels = selected_areas_and_labels$labels,
  affected_areas_colour = init_vars$affected_areas_colour, reporting_lag_start = init_vars$reporting_lag_start,
  knn_neighbours = init_vars$knn_neighbours, surrounding_areas = init_vars$surrounding_areas,
  log_const = init_vars$log_const, prop_tests_in_area = init_vars$prop_tests_in_area,
  counterfactual_areas_colours = init_vars$counterfactual_areas_colours, reporting_lag_end = init_vars$reporting_lag_end,
  individual_series = init_vars$individual_series, synthetic_counterfactual_colour = init_vars$synthetic_counterfactual_colour,
  ci_start = init_vars$ci_start, use_common_legend = init_vars$use_common_legend, 
  grid_plot_height = 16, grid_plot_width = 12, save_raw_data_graphs = TRUE,
  save_ci_graphs = TRUE,
  df_lfd = initialised_data$df_lfd, df_pcr = initialised_data$df_pcr,
  init_data = initialised_data$init_data, save_line_plots = TRUE,
  line_plot_height = 8, line_plot_width = 12,
  df_bubble = initialised_data$df_bubble,
  save_bubble_map = TRUE, bubble_map_height = 20, bubble_map_width = 14,
  rest_of_england_map_df = initialised_data$df_geospatial_map$rest_of_england_map_df,
  affected_df = initialised_data$df_geospatial_map$affected_df,
  counterfactual_df = initialised_data$df_geospatial_map$counterfactual_df,
  geospatial_map_height = 14,
  geospatial_map_width = 20,
  save_map = TRUE, save_joined_plots = TRUE,
  joined_plot_height = 15, joined_plot_width = 24,
  ltla_pcr_daily_growth_rate_percent = initialised_data$ltla_pcr_daily_growth_rate_percent,
  ltla_lfd_daily_growth_rate_percent = initialised_data$ltla_lfd_daily_growth_rate_percent,
  daily_growth_rate_ltla_save = TRUE
)

# create all the tables present in the preprint
tables <- finalise_tables_production(
  surrounding_areas = init_vars$surrounding_areas, labels = selected_areas_and_labels$labels,
  impact_measure_with_end_date = init_vars$impact_measure_with_end_date,
  reporting_lag_start = init_vars$reporting_lag_start, data = initialised_data$data,
  prop_tests_in_area = init_vars$prop_tests_in_area, prop_tests_lab_x = init_vars$prop_tests_lab_x,
  knn_neighbours = init_vars$knn_neighbours, rest_of_england = init_vars$rest_of_england,
  selected_areas = selected_areas_and_labels$selected_areas,  
  individual_series = init_vars$individual_series, apply_log = init_vars$apply_log, log_const = init_vars$log_const,
  utla_col_name = "All affected areas", coeff_lfdpos_caserates = init_vars$coeff_lfdpos_caserates,
  pcr_df = initialised_data$df_pcr
)

# Create R markdown file
render(
  params = list(
    figure_1 = "../visuals/geospatial_maps/joined_plot.png",
    figure_2 = visuals$line_plots$daily_LFD_positivity, #"../visuals/line_plots/LFD_positivity",#
    figure_3 = visuals$lfd_positivity$final_initial_data_plot,
    figure_4 = visuals$admissions_per_100k$final_initial_data_plot,
    figure_5 = visuals$deaths_per_100k$final_initial_data_plot,
    figure_6 = visuals$lfd_positivity$final_ci_plot,
    figure_7 = visuals$admissions_per_100k$final_ci_plot,
    figure_8 = visuals$deaths_per_100k$final_ci_plot,
    figure_9 = visuals$LFD_ltla_growth_rate,
    table_1 = tables$table_1,
    table_2 = tables$table_2_reporting_channel_breakdown,
    table_3 = tables$table_3_false_negatives_table,
    table_4 = tables$table_4,
    table_5 = tables$table_5,
    table_6 = tables$table_6
  ),
  input = "outputs/report/preprint_rmarkdown.Rmd",
  output_file = "UKHSA_immensa_preprint.docx"
)

