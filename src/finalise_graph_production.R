#' finalise_graph_production
#'
#' `finalise_graph_production()` Produces all graphs used for the preprint
#'
#' @param impact_measure_with_end_date End date of the impact
#'
#' @param prop_tests_lab_x List of proportion of tests 
#' 
#' @param rest_of_england bool - use the rest of england for Causal Impact Analysis
#' 
#' @param selected_areas List - Affected Area and their respective counterfactual areas
#' 
#' @param data data.frame - Summary Data, `initialised_data$data`
#' 
#' @param apply_log bool - Apply log transformation
#' 
#' @param labels list - A list of titles for detailing the proportion of tests by affected area
#' 
#' @param affected_areas_colour str - Colour to plot the affected areas
#' 
#' @param reporting_lag_start date - date the reporting lag started
#' 
#' @param knn_neighbours int - Number of KNN neighours to use for Causal Impact analysis
#'  
#' @param surrounding_areas List of affected areas and their counterfactuals
#' 
#' @param log_const int - the log constant
#' 
#' @param prop_tests_in_area List of the proportion of tests in the affected areas
#' 
#' @param counterfactual_areas_colours str - Colour to plot the coutnerfactual areas
#' 
#' @param reporting_lag_end date - date the reporting lag ended
#' 
#' @param individual_series bool - Utilise individual series or not
#' 
#' @param synthetic_counterfactual_colour str - Colour to plot the synthetic counterfactual areas
#' 
#' @param ci_start date - Date to start Causal Impact Analysis
#' 
#' @param use_common_legend bool - Use a common legend in all the Causal Impact Plots
#' 
#' @param grid_plot_height int - height of saved plots
#' 
#' @param grid_plot_width int - width of saved plots
#' 
#' @param save_raw_data_graphs bool - save out raw graphs
#' 
#' @param save_ci_graphs bool - save out causal impact graph
#' 
#' @param df_lfd data.frame - Lateral Flow Testing Data, `initialised_data$df_lfd`
#' 
#' @param df_pcr data.frame - Polymerase Chain Reaction Testing Data `initialised_data$df_pcr`
#' 
#' @param init_data data.frame - Summary Data, `initialised_data$init_data`
#' 
#' @param save_line_plots bool
#' 
#' @param line_plot_height int - height of saved plots
#' 
#' @param line_plot_width int - width of saved plots
#' 
#' @param df_bubble data.frame - Proportion of tests undertaken by the Laboratory used to create the bubble map, `initialised_data$df_bubble`
#' 
#' @param save_bubble_map bool - Save bubble map
#' 
#' @param bubble_map_height int - height of bubble map
#' 
#' @param bubble_map_width int - width of bubble map
#' 
#' @param rest_of_england_map_df data.frame - UTLA's which are not affected or counterfactual areas
#' 
#' @param affected_df data.frame - UTLA's which are affected 
#' 
#' @param counterfactual_df data.frame - UTLA's which are counterfactuals
#' 
#' @param geospatial_map_height int - height of Affected to Counterfactual map
#' 
#' @param geospatial_map_width int - width of Affected to Counterfactual map
#' 
#' @param save_map bool - Save the Affected to Counterfactual map
#' 
#' @param save_joined_plots bool - Required to create the preprint - Save the combined bubble map and Affected to Counterfactual map
#' 
#' @param joined_plot_height int - height of Joined map
#' 
#' @param joined_plot_width int - width of Joined map

finalise_graph_production <- function(impact_measure_with_end_date, prop_tests_lab_x,
                                      rest_of_england, selected_areas,
                                      data, apply_log, revert_log, 
                                      labels, affected_areas_colour,
                                      reporting_lag_start, knn_neighbours,
                                      surrounding_areas, log_const,
                                      prop_tests_in_area, counterfactual_areas_colours,
                                      reporting_lag_end,
                                      individual_series, synthetic_counterfactual_colour,
                                      ci_start,
                                      use_common_legend, 
                                      grid_plot_height, grid_plot_width,
                                      save_raw_data_graphs,
                                      save_ci_graphs,
                                      df_lfd, df_pcr, init_data, save_line_plots,
                                      line_plot_height, line_plot_width,
                                      df_bubble, save_bubble_map, bubble_map_height, bubble_map_width,
                                      rest_of_england_map_df, affected_df,
                                      counterfactual_df, geospatial_map_height, geospatial_map_width,
                                      save_map, save_joined_plots,
                                      joined_plot_height, joined_plot_width, 
                                      ltla_pcr_daily_growth_rate_percent,ltla_lfd_daily_growth_rate_percent, 
                                      daily_growth_rate_ltla_save) {
  outputs <- list()
  # add more comments
  for (impact_measure in names(impact_measure_with_end_date)) {
    # process and create datasets for each impact measure
  
    data_comb_df <- process_data(
      impact_measure, data, prop_tests_in_area,
      prop_tests_lab_x, surrounding_areas, knn_neighbours,
      rest_of_england, selected_areas
    )
  
    # process and run causal impact for each impact measure
    ci_data_sets <- causal_impact_data_generation(
      selected_areas, data, impact_measure,
      data_comb_df, reporting_lag_start, individual_series,
      apply_log, log_const, rest_of_england
    )
 
    # create grid plot
    final_initial_data_plot <- merge_initial_data_grid_fig(
      data_comb_df, reporting_lag_start, reporting_lag_end,
      labels, prop_tests_in_area, prop_tests_lab_x,
      affected_areas_colour, counterfactual_areas_colours,
      use_common_legend, impact_measure,  grid_plot_height, grid_plot_width,
      save_raw_data_graphs
    )
    
    # Plotting causal impact modelling
    # Preparing the data to plot all Affected areas and Counterfactuals
    # issue sits in here for when indivudal series is FALSE
    final_ci_plot <- merge_ci_model_grid_fig(
      ci_data_sets$data_new, ci_data_sets$ci_new, prop_tests_in_area, prop_tests_lab_x,
      reporting_lag_start, reporting_lag_end, ci_start,
      selected_areas, rest_of_england, surrounding_areas,
      knn_neighbours, individual_series, data,
      apply_log, log_const, use_common_legend,
      affected_areas_colour, synthetic_counterfactual_colour,
      impact_measure, labels,  grid_plot_height, grid_plot_width, save_ci_graphs, data_comb_df, revert_log = revert_log
    )
    # return causal impact and comparison plots for each impact measure
    results <- list(
      "final_initial_data_plot" = final_initial_data_plot,
      "final_ci_plot" = final_ci_plot
    )
  
    outputs[[impact_measure]] <- results
  }

  outputs[["line_plots"]] <- produce_lfd_plot(
    df_lfd, df_pcr, init_data,
    reporting_lag_start, reporting_lag_end,
    save_line_plots, line_plot_height, line_plot_width
  )

  outputs[["bubble_map"]] <- plot_bubble_map(
    df_bubble,
    save_bubble_map, bubble_map_height, bubble_map_width
  )

  outputs[["geospatial_map"]] <- plot_affected_to_counterfactual_map(
    rest_of_england_map_df, affected_df,
    counterfactual_df,
    geospatial_map_height, geospatial_map_width,
    save_map
  )

  outputs[["joined_plot"]] <- join_plots(
    outputs[["bubble_map"]], outputs[["geospatial_map"]], save_joined_plots,
    joined_plot_height, joined_plot_width
  )

  outputs[["LFD_ltla_growth_rate"]] <- plot_ltla_region_swash(
    ltla_lfd_daily_growth_rate_percent,
    c("South East", "South West", "West Midlands"),test_type= 'LFD', save = daily_growth_rate_ltla_save
  )
  
  return(outputs)
}
