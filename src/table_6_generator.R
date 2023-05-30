#' table_6_generator
#'
#' `table_6_generator()` 
#'
#' @param impact_measure_with_end_date 
#'
#' @param data 
#'
#' @param prop_tests_in_area 
#' 
#' @param prop_tests_lab_x 
#' 
#' @param knn_neighbours 
#' 
#' @param selected_areas
#' 
#' @param rest_of_england
#' 
#' @param reporting_lag_start
#' 
#' @param individual_series
#' 
#' @param apply_log
#' 
#' @param log_const
#' 
#' @param utla_col_name
#' 
#' @param coeff_lfdpos_caserates


table_6_generator <- function(impact_measure_with_end_date,
                               data, prop_tests_in_area,
                               prop_tests_lab_x, surrounding_areas,
                               rest_of_england, selected_areas,
                               reporting_lag_start, individual_series,
                               apply_log, log_const,
                               utla_col_name, coeff_lfdpos_caserates) {
  output <- list()
  headers <- list()

  output[["KNN (5 neighbours)"]] <- table_row_values(
    impact_measure_with_end_date,
    data, prop_tests_in_area,
    prop_tests_lab_x, FALSE,
    knn_neighbours = 5, FALSE,
    selected_areas, reporting_lag_start,
    individual_series, apply_log, log_const, utla_col_name, coeff_lfdpos_caserates
  )
  
  headers[["KNN (5 neighbours)"]] <- "KNN ( 5 neighbours)"
  
  # load labels with the surrounding area df for the surrounding column in the last table
  # this is used for table 6 where the surrounding area column has been created
  # move this into table 6 function
  selected_areas_and_labels_surrounding <- initialise_selected_areas_and_labels(
    init_vars$prop_tests_in_area,
    init_vars$prop_tests_lab_x,
    initialise_areas(TRUE),
    init_vars$rest_of_england
  )
 
  output[["Surrounding Areas"]] <- table_row_values(
    impact_measure_with_end_date,
    data, prop_tests_in_area,
    prop_tests_lab_x,
    surrounding_areas = TRUE,
    knn_neighbours = FALSE, rest_of_england = FALSE,
    selected_areas_and_labels_surrounding$selected_areas, reporting_lag_start,
    individual_series, apply_log, log_const, utla_col_name, coeff_lfdpos_caserates
  )

  headers[["Surrounding Areas"]] <- "Surrounding Areas"
  
  output[["Rest of England"]] <- table_row_values(
    impact_measure_with_end_date,
    data, prop_tests_in_area,
    prop_tests_lab_x,
    surrounding_areas = FALSE,
    knn_neighbours = FALSE, rest_of_england = TRUE,
    selected_areas, reporting_lag_start,
    individual_series, apply_log, log_const, utla_col_name, coeff_lfdpos_caserates
  )
  
  headers[["Rest of England"]] <- "Rest of England"

  table <- as.data.frame(c("LFD Positivity", "Admissions", "Deaths")) %>%
    cbind(as.data.frame(t(as.matrix(bind_rows(output)))))


  names(table) <- c("Model", headers)
  row.names(table) <- NULL

  return(table)
}

#' table_row_values
#'
#' `table_row_values()` 
#'
#' @param impact_measure_with_end_date 
#'
#' @param data 
#'
#' @param prop_tests_in_area 
#' 
#' @param prop_tests_lab_x 
#' 
#' @param knn_neighbours 
#' 
#' @param selected_areas
#' 
#' @param reporting_lag_start
#' 
#' @param individual_series
#' 
#' @param apply_log
#' 
#' @param log_const
#' 
#' @param utla_col_name
#' 
#' @param coeff_lfdpos_caserates

table_row_values <- function(impact_measure_with_end_date,
                             data, prop_tests_in_area,
                             prop_tests_lab_x, surrounding_areas,
                             knn_neighbours, rest_of_england,
                             selected_areas, reporting_lag_start,
                             individual_series, apply_log, log_const,
                             utla_col_name, coeff_lfdpos_caserates) {
  outputs <- list()
  for (impact_measure in names(impact_measure_with_end_date)) {
  
    data_comb_df_ci <- process_data(
      impact_measure,
      data, prop_tests_in_area,
      prop_tests_lab_x, surrounding_areas,
      knn_neighbours, rest_of_england,
      selected_areas
    )
    
    ci_data_sets <- causal_impact_data_generation(
      selected_areas, data, impact_measure,
      data_comb_df_ci, reporting_lag_start, individual_series,
      apply_log, log_const, rest_of_england
    )
    
    text <- table_value_output(
      data, ci_data_sets$ci_data, ci_data_sets$ci,
      utla_col_name,
      reporting_lag_start, apply_log,
      impact_measure, log_const,
      impact_measure_with_end_date[[impact_measure]],
      coeff_lfdpos_caserates
    )

    outputs[[impact_measure]] <- text
  }
  
  return(outputs)
}

#' table_value_output
#'
#' `table_value_output()` Prints out all the summary statistics
#'
#' @param data 
#'
#' @param ci_data 
#' 
#' @param ci 
#' 
#' @param utla_col_name 
#' 
#' @param reporting_lag_start
#' 
#' @param impact_measure
#' 
#' @param apply_log
#' 
#' @param log_const
#' 
#' @param end_date
#' 
#' @param coeff_lfdpos_caserates

table_value_output <- function(data, ci_data, ci, utla_col_name,
                               reporting_lag_start, apply_log,
                               impact_measure,
                               log_const, end_date,
                               coeff_lfdpos_caserates) {
  ci <- as.data.frame(ci$series) %>%
    cbind(date = rownames(.), .)

  if (apply_log == TRUE) {
    data <- ci_data$time_df %>%
      as.data.frame() %>%
      cbind(date = rownames(.), .) %>%
      lapply(exp_fun, log_const) %>%
      as.data.frame() %>%
      rename(`All affected areas` = All.affected.areas)
    ci <- as.data.frame(lapply(ci, exp_fun, log_const))
  }

  if (impact_measure == "lfd_positivity") {
    metric <- "%"
    metric_diff <- "pp"
    weekly <- FALSE
  } else if ((impact_measure == "admissions_per_100k") |
    (impact_measure == "deaths_per_100k")) {
    metric <- "per 100k"
    metric_diff <- "per 100k"
    weekly <- FALSE
  } else if ((impact_measure == "7day_admissions_per_100k") |
    (impact_measure == "7day_deaths_per_100k")) {
    metric <- "per 100k"
    metric_diff <- "per 100k"
    weekly <- TRUE
  }

  data <- data[data$date <= end_date, ]
  ci <- ci[ci$date <= end_date, ]

  avg_impact <- mean(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred)
  avg_impact_lower <- mean(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred.upper)
  avg_impact_upper <- mean(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred.lower)

  total_impact <- sum(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred)
  total_impact_lower <- sum(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred.upper)
  total_impact_upper <- sum(data[data$date >= reporting_lag_start, ][[utla_col_name]] - ci[ci$date >= reporting_lag_start, ]$point.pred.lower)
 
  if (impact_measure == "lfd_positivity") {
    total_impact <- total_impact * coeff_lfdpos_caserates
    total_impact_lower <- total_impact_lower * coeff_lfdpos_caserates
    total_impact_upper <- total_impact_upper * coeff_lfdpos_caserates

  }

  if (weekly == TRUE) {
    total_impact <- total_impact / 7
    total_impact_lower <- total_impact_lower / 7
    total_impact_upper <- total_impact_upper / 7
  }
  
  # population of affected areas
  pop <- 3.25553E6
 

  if (impact_measure == "lfd_positivity") {
    days <- 59
    coeff <- 20
    
    output <- paste(as.character(round(avg_impact, 1)), metric_diff, "\n[", as.character(round(avg_impact_lower, 1)), "-", as.character(round(avg_impact_upper, 1)), "]\n")
    
    calculation_central <- avg_impact  * days * coeff / 1E5 * pop
    calculation_lower <- avg_impact_lower * days * coeff / 1E5  * pop
    calculation_upper <- avg_impact_upper * days * coeff / 1E5  * pop
  
    add_string <- paste0(as.character(comma(ceiling(calculation_central))), 
                         " additional cases \n[", as.character(comma(ceiling(calculation_lower))), 
                         " - ", as.character(comma(ceiling(calculation_upper))), "]\n")
    output <- paste0(output, "\n", add_string, "\n")
  } 
  else {
    if (impact_measure == "admissions_per_100k") {
      substring <- "admissions"
    } else {
      substring <- "deaths"
    }

    output <- paste(as.character(round(total_impact, 1)), metric_diff, "\n[", as.character(round(total_impact_lower, 1)), "-", as.character(round(total_impact_upper, 1)), "]\n")
    calculation_central <- total_impact * pop / 1E5
    calculation_lower <- total_impact_lower * pop / 1E5
    calculation_upper <- total_impact_upper * pop / 1E5

    add_string <- glue("{as.character(comma(ceiling(calculation_central)))} additional {substring}\n[{as.character(comma(ceiling(calculation_lower)))} - {as.character(comma(ceiling(calculation_upper)))}]")
    output <- paste0(output, "\n", add_string)
  }
  return(output)
}


