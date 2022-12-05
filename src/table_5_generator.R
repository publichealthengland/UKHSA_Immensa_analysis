# Generates a report relating to the average positivity
table_5_row_gen <- function(data_comb_df, impact_measure, reporting_lag_start, end_date, coeff_lfdpos_caserates) {
  # dont think there should be any NAs
  data_comb_df_drop_na <- drop_na(data_comb_df)
  
  # get the average of all affected areas
  affected_post <- data_comb_df_drop_na[(data_comb_df_drop_na$date >= reporting_lag_start) & 
                                          (data_comb_df_drop_na$date <= end_date), ][["All affected areas"]]
  # get the average of all counterfactual areas
  counterfactual_post <- data_comb_df_drop_na[(data_comb_df_drop_na$date >= reporting_lag_start) & 
                                                (data_comb_df_drop_na$date <= end_date), ][["All counterfactual areas"]]
  # get the differences between the 2 columns
  diff_post <- affected_post - counterfactual_post
  # get the average
  avg_diff_post <- mean(diff_post)
  
  # the same for the pre period
  affected_pre <- data_comb_df_drop_na[data_comb_df_drop_na$date < reporting_lag_start, ][["All affected areas"]]
  counterfactual_pre <- data_comb_df_drop_na[data_comb_df_drop_na$date < reporting_lag_start, ][["All counterfactual areas"]]
  
  diff_pre <- affected_pre - counterfactual_pre
  
  avg_diff_pre <- mean(diff_pre)
  
  # adjusts by subtracting the 2 averages
  adjusted_diff <- avg_diff_post - avg_diff_pre
  
  # convert the date diff to a number
  days <- as.numeric(end_date - reporting_lag_start)
  
  # pooled standard deviation
  pooled_sdev <- sd_pooled(diff_post, diff_pre)

  # Cumulative differences
  sdev_pre <- sd(diff_pre)
  
  if (impact_measure == "lfd_positivity") {
    cum_diff_post <- mean(affected_post - counterfactual_post) * coeff_lfdpos_caserates * days
    avg_diff_pre <- avg_diff_pre * coeff_lfdpos_caserates
    adjustment_factor <- avg_diff_pre * days
    adjustment_factor_lower <- (avg_diff_pre + 2 * sdev_pre) * coeff_lfdpos_caserates * days
    adjustment_factor_upper <- (avg_diff_pre - 2 * sdev_pre) * coeff_lfdpos_caserates * days
    
  } else {

    cum_diff_post <- sum(affected_post - counterfactual_post)
    adjustment_factor <- avg_diff_pre * days
    adjustment_factor_lower <- (avg_diff_pre - 2 * sdev_pre) * days
    adjustment_factor_upper <- (avg_diff_pre + 2 * sdev_pre) * days
  }
  adj_cum_diff_post <- cum_diff_post - adjustment_factor
  adj_cum_diff_post_lower <- cum_diff_post - adjustment_factor_upper
  adj_cum_diff_post_upper <- cum_diff_post - adjustment_factor_lower
 
  if (adjustment_factor_lower > adjustment_factor_upper) {
    temp <- adjustment_factor_lower
    adjustment_factor_lower <- adjustment_factor_upper
    adjustment_factor_upper <- temp
  }
  if (adj_cum_diff_post_lower > adj_cum_diff_post_upper) {
    temp <- adj_cum_diff_post_lower
    adj_cum_diff_post_lower <- adj_cum_diff_post_upper
    adj_cum_diff_post_upper <- temp
  }

  pop <- 3.25553E6
  row1 <- round(cum_diff_post, 1)
  row2 <- round(avg_diff_pre, 3)
  row3 <- days
  row4 <- round(adjustment_factor, 1)
  row5 <- round(adj_cum_diff_post, 1)
  row6 <- comma(ceiling(round(adj_cum_diff_post, 1) * pop / 1E5))

  return(list(row1, row2, row3, row4, row5, row6))
}

table_5_col_generator <- function(impact_measure, reporting_lag_start, end_date, data, prop_tests_in_area,
                                  prop_tests_lab_x, surrounding_areas, knn_neighbours,
                                  rest_of_england, selected_areas) {
  data_comb_df <- process_data(
    impact_measure, data, prop_tests_in_area,
    prop_tests_lab_x, surrounding_areas, knn_neighbours,
    rest_of_england, selected_areas
  )

  central <- table_5_row_gen(
    data_comb_df,
    impact_measure, reporting_lag_start, end_date,
    20.0
  )

  if (impact_measure == "lfd_positivity") {
    add_string <- "cases"
  } else if (impact_measure == "admissions_per_100k") {
    add_string <- "admissions"
  } else if (impact_measure == "deaths_per_100k") {
    add_string <- "deaths"
  }

  if (impact_measure == "lfd_positivity") {

    return(
      paste0(
      central,
      list(" per 100k (2)", " per 100k", "", " per 100k", " per 100k", paste(" additional", add_string))
    ))
  } else {
    return(paste(paste0(
      central,
      list(" per 100k", " per 100k", "", " per 100k", " per 100k", paste(" additional", add_string))
    )))
  }
}

final_table_5_generator <- function(impact_measure_with_end_date,
                                    reporting_lag_start,
                                    data,
                                    prop_tests_in_area,
                                    prop_tests_lab_x,
                                    surrounding_areas,
                                    knn_neighbours,
                                    rest_of_england,
                                    selected_areas) {
  cols <- list()

  cols[["Measure"]] <- c(
    "Cumulative difference in post period (unadjusted) (a)",
    "Average daily divergence in pre-period (1st June to 1st September) (1) (b)",
    "Days in post-period (c)",
    "Expected difference based on pre-period differences (d=b*c)",
    "Adjusted cumulative difference in post period (e=a-d)",
    "Cases / admissions / deaths (f=e/100k*population)"
  )

  for (impact_measure in names(impact_measure_with_end_date)) {
   
    cols[[impact_measure]] <- table_5_col_generator(
      impact_measure, reporting_lag_start,
      impact_measure_with_end_date[[impact_measure]], data,
      prop_tests_in_area,
      prop_tests_lab_x, surrounding_areas,
      knn_neighbours, rest_of_england,
      selected_areas
    )
  }

  table <- as.data.frame(bind_rows(cols))

  names(table) <- c("Measure", "Cases", "Hospitalisations", "Deaths")

  return(table)
}
