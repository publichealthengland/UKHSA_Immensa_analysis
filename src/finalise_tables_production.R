#' finalise_tables_production
#'
#' `finalise_tables_production()` Creates all the tables used within the preprint
#'
#' @param surrounding_areas: List of affected areas and their counterfactuals
#' 
#' @param labels: List of the affected areas and the proportion of tests undertaken in the area
#' 
#' @param impact_measure_with_end_date: End date of the impact
#' 
#' @param reporting_lag_start: When the reporting lag started
#' 
#' @param data: data.frame stored in `initialised_data$data` which is the Summary Data
#' 
#' @param prop_tests_in_area: List of the proportion of tests in the affected areas
#' 
#' @param prop_tests_lab_x: List of proportion of tests 
#' 
#' @param knn_neighbours: int - Number of KNN neighours to use for Causal Impact analysis
#' 
#' @param rest_of_england: bool - use the rest of england for Causal Impact Analysis
#' 
#' @param selected_areas: List - Affected Area and their respective counterfactual areas
#' 
#' @param individual_series: bool - Used for Table A2 [MAY NEED TO CHANGE NAME]
#' 
#' @param apply_log: bool - Apply log transformation
#' 
#' @param log_const: int - the log constant
#' 
#' @param utla_col_name: str - Name of column
#' 
#' @param coeff_lfdpos_caserates: int - Rate used in Table A2 [MAY NEED TO CHANGE TABLE NAME]
#' 
#' @param pcr_df: data.frame - PCR Data stored in `initialised_data$df_pcr`

finalise_tables_production <- function(surrounding_areas, labels,
                                       impact_measure_with_end_date,
                                       reporting_lag_start, data,
                                       prop_tests_in_area, prop_tests_lab_x,
                                       knn_neighbours, rest_of_england, selected_areas,
                                       individual_series, apply_log, log_const,
                                       utla_col_name, coeff_lfdpos_caserates, pcr_df) {
  
  table_1 <-
    selected_areas %>%
    separate(Counterfactual.areas, c(
      "Nearest Neighbour 1", "Nearest Neighbour 2",
      "Nearest Neighbour 3", "Nearest Neighbour 4",
      "Nearest Neighbour 5"
    ), ", ") %>%
    rename(`Affected area` = Affected.areas)
  
  table_2_reporting_channel_breakdown <- table_2_produce_pcr_tests_result_reporting_breakdown(pcr_df)
  
  table_3_false_negatives_table <- table_3_produce_false_negative_table(pcr_df)
  
  table_4 <- table_4_generator(surrounding_areas, labels)
  
  table_5 <- final_table_5_generator(
    impact_measure_with_end_date, reporting_lag_start,
    data, prop_tests_in_area, prop_tests_lab_x,
    surrounding_areas, knn_neighbours,
    rest_of_england, selected_areas
  )
  
  table_6 <- table_6_generator(
    impact_measure_with_end_date,
    data, prop_tests_in_area,
    prop_tests_lab_x, surrounding_areas,
    rest_of_england,
    selected_areas, reporting_lag_start,
    individual_series, apply_log,
    log_const, utla_col_name, coeff_lfdpos_caserates
  )

  return(list(
    "table_4" = table_4,
    "table_5" = table_5,
    "table_1" = table_1,
    "table_6" = table_6,
    "table_3_false_negatives_table" = table_3_false_negatives_table,
    "table_2_reporting_channel_breakdown" = table_2_reporting_channel_breakdown
  ))
}
