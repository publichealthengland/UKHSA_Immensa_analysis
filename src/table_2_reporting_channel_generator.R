#' finalise_graph_production
#'
#' `group_find_positivity_p_val()` Obtains the P value for the proportion of test undertaken by Immensa 
#'
#' @param df_pcr data.frame - Polymerase Chain Reaction Testing Data `initialised_data$df_pcr`
#' 
#' @param groupby_col str - column name to group by
#' 
group_find_positivity_p_val <- function(df, groupby_col) {
  df_grouped <- df %>%
    select(c(
      groupby_col, "count_pcr_tests_lab_x", "count_pcr_tests_rest_of_england", "count_positive_pcr_tests_lab_x",
      "count_positive_pcr_tests_rest_of_england",
      "count_negative_pcr_tests_lab_x","count_negative_pcr_tests_rest_of_england"
    )) %>%
    group_by(across(groupby_col)) %>%
    summarise(across(everything(), sum))
  df_grouped$total_affected_tests <- df_grouped$count_positive_pcr_tests_lab_x + df_grouped$count_negative_pcr_tests_lab_x

  df_grouped$total_ROE_tests <- df_grouped$count_positive_pcr_tests_rest_of_england + df_grouped$count_negative_pcr_tests_rest_of_england

  df_grouped$lab_x_positivity <- (df_grouped$count_positive_pcr_tests_lab_x / df_grouped$total_affected_tests)

  df_grouped$lab_x_prop <- (df_grouped$count_positive_pcr_tests_lab_x + df_grouped$count_negative_pcr_tests_lab_x) /
    sum(df_grouped$total_affected_tests)

  df_grouped$ROE_positivity <- (df_grouped$count_positive_pcr_tests_rest_of_england /
    df_grouped$total_ROE_tests)

  df_grouped$ROE_prop <- (df_grouped$count_positive_pcr_tests_rest_of_england + df_grouped$count_negative_pcr_tests_rest_of_england) /
    sum(df_grouped$total_ROE_tests)

  df_grouped$p_value <- suppressWarnings(mapply(
    obtain_p_value, df_grouped$count_negative_pcr_tests_lab_x,
    df_grouped$count_negative_pcr_tests_rest_of_england,
    df_grouped$count_positive_pcr_tests_lab_x,
    df_grouped$count_positive_pcr_tests_rest_of_england
  ))
  names(df_grouped)[names(df_grouped) == groupby_col] <- "reporting_channel"
  df_grouped <- df_grouped %>%
    mutate(
      lab_x_positivity = ifelse(lab_x_positivity < 0.01, "<1%",
        percent(lab_x_positivity, accuracy = 0.1)
      ),
      lab_x_prop = ifelse(lab_x_prop < 0.01, "<1%", percent(lab_x_prop, accuracy = 0.1)),
      ROE_positivity = percent(ROE_positivity, accuracy = 0.1),
      ROE_prop = percent(ROE_prop, accuracy = 0.1),
      count_pcr_tests_lab_x = comma_format()(count_pcr_tests_lab_x),
      count_pcr_tests_rest_of_england = comma_format()(count_pcr_tests_rest_of_england)
    ) %>%
    select(!c("total_affected_tests", "total_ROE_tests"))
  return(df_grouped)
}


obtain_p_value <- function(count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
                           count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england) {
  prop_matrix <- matrix(c(
    count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
    count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england
  ),
  ncol = 2
  )
  #     return(prop_matrix)
  p_val <- prop.test(prop_matrix)$p.value
  return(ifelse(p_val <= 0.001, "< 0.001", round(p_val, 4)))
}

#' produce_pcr_tests_result_reporting_breakdown
#'
#' `produce_pcr_tests_result_reporting_breakdown()` 
#'
#' @param df_pcr data.frame - Polymerase Chain Reaction Testing Data `initialised_data$df_pcr`
#' 
#' @return data.frame of the results false negatives and confidence intervals for each value
#' 
table_2_produce_pcr_tests_result_reporting_breakdown <- function(df) {
  df$specimen_processed_date <- as.Date(df$specimen_processed_date)
  df <- df %>% filter((specimen_processed_date >= "2021-09-02") &
    (specimen_processed_date <= "2021-10-12"))

  df_age_reporting_function <- group_find_positivity_p_val(df, "age_group")

  df_channel_reporting <- group_find_positivity_p_val(df, "test_site")
  df_channel_reporting[df_channel_reporting$reporting_channel == "drivein", "reporting_channel"] <- "Drive-in"
  df_channel_reporting[df_channel_reporting$reporting_channel == "home", "reporting_channel"] <- "Home and residential settings* "
  df_channel_reporting[df_channel_reporting$reporting_channel == "unknown", "reporting_channel"] <- "Unknown"
  df_channel_reporting[df_channel_reporting$reporting_channel == "walkin", "reporting_channel"] <- "Walk-in"

  df_region_reporting <- group_find_positivity_p_val(df, "region")

  df_final <- rbind(
    c("Age", "", "", "", "", "", "", "", "", "", "", ""), df_age_reporting_function,
    c("", "", "", "", "", "", "", "", "", "", "", ""),
    c("Region", "", "", "", "", "", "", "", "", "", "", ""), df_region_reporting,
    c("", "", "", "", "", "", "", "", "", "", "", ""),
    c("Test Site", "", "", "", "", "", "", "", "", "", "", ""), df_channel_reporting
  ) %>%
    select(c(
      "reporting_channel", "count_pcr_tests_lab_x", "lab_x_prop", "lab_x_positivity",
      "count_pcr_tests_rest_of_england", "ROE_prop", "ROE_positivity", "p_value"
    )) # %>%
  names(df_final) <- c(
    "Reporting Channel", "Number of Tests", "Proportion", "Positivity",
    "Number of Tests ", "Proportion ", "Positivity ", "P Value"
  )

  return(df_final)
}
