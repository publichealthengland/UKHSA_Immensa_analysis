#'
#' `process_data` 
#'
#' @param impact_measure: lfd_positivity, admissions_per_100k, 7day_admissions_per_100k, deaths_per_100k, 7day_deaths_per_100k
#'
#' @param data: the raw datasets that have been grouped by utla_code, utla, region and date
#' 
#' @param prop_tests_in_area: List of proportion of tests in the area
#' 
#' @param prop_tests_lab_x: List of proportion of tests in the lab x
#' 
#' @param surrounding_areas: List of affected areas and their counterfactuals
#' 
#' @param knn_neighbours: int - Number of KNN neighours to use for Causal Impact analysis
#' 
#' @param rest_of_england: bool - use the rest of england for Causal Impact Analysi
#' 
#' @param selected_areas: List of affected Area and their respective counterfactual areas
#' 
#' 

process_data <- function(impact_measure, data, prop_tests_in_area,
                         prop_tests_lab_x, surrounding_areas, knn_neighbours,
                         rest_of_england, selected_areas) {
  
  aggFunc <- graph_arg_generation(impact_measure)
  
  data_comb_df <- as.data.frame(unique(data[["date"]]))
  names(data_comb_df) <- "date"

  # Combine the affected areas with their respective counterfactuals
  # The aggregation function (aggFunc) is applied to do the aggregation
  # as generated above
  number_affected_areas <- dim(selected_areas)[[1]]
  for (i in 1:number_affected_areas) {
    affected <- selected_areas[["Affected.areas"]][[i]]
    counterfactual <- as.list(strsplit(selected_areas[["Counterfactual.areas"]][[i]], ", "))[[1]]
    # if rest of england is false get the cf row from the dataframe
    if (rest_of_england == FALSE) {
    #   
    # 
       if (surrounding_areas == FALSE) {
        counterfactual <- counterfactual[1:knn_neighbours]
       }
        # else {
    #     counterfactual <- counterfactual#[1:length(counterfactual) - 1]
    # }
    # 
    #   # Selected area's counterfactuals are replaced with the ones being
    #   # considered only
      selected_areas[["Counterfactual.areas"]][[i]] <- toString(counterfactual)
    # 
    # #   # cat(paste(affected, "with", toString(counterfactual), "\n\n"))
    }

    # Produces an aggregated data_comb_df with the impact measure for the
    # affected area
    # filter onto only the affected area within the data
    # group by date - sum where numeric - aggregate using the function for the specific impact measure
    affected_agg <-
      data[(data$utla == affected), ] %>%
      group_by(date) %>%
      summarise(across(where(is.numeric), sum),
        .groups = "drop"
      ) %>%
      aggFunc(impact_measure) %>%
      select(date, starts_with(impact_measure)) %>%
      rename("{affected}" := impact_measure)
    
    #  for counterfactual df
    if (rest_of_england == FALSE) {
      
      # filter onto the counterfactual UTLA's from the list above
      counterfactual_agg <- data[data$utla %in%
        counterfactual, ]
    } else {
      # if rest of england is true the counterfactual UTLAs are every utla which is not affected
      counterfactual_agg <- data[data$utla %ni%
                    selected_areas[["Affected.areas"]], ]
    }

    # Repeats the process for counterfactual areas
    counterfactual_agg <- counterfactual_agg %>%
      group_by(date) %>%
      summarise(across(where(is.numeric), sum),
        .groups = "drop"
      ) %>%
      aggFunc(impact_measure) %>%
      select(date, starts_with(impact_measure)) %>%
      rename("{affected}_CF" := impact_measure)

    # Merges the result onto the initialised data_comb_df
    data_comb_df <- data_comb_df %>%
      full_join(affected_agg, by = "date") %>%
      full_join(counterfactual_agg, by = "date")
  }
  # repreat process at an aggregate level
  # List of all affected areas
  affected_areas <- selected_areas[["Affected.areas"]]

  # Gets the unique counterfactual areas as a list for the following print
  counterfactual_areas <- unique(str_split(toString(selected_areas[["Counterfactual.areas"]]), ", ")[[1]])

  # Applies aggregation to the affected areas based on impact measure
  affected_agg <-
    data[(data$utla %in% affected_areas), ] %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), sum),
      .groups = "drop"
    ) %>%
    aggFunc(impact_measure) %>%
    select(date, starts_with(impact_measure)) %>%
    rename("All affected areas" := impact_measure)

  # Extracts the counterfactual areas to aggregate against
  if (rest_of_england == FALSE) {
    counterfactual_agg <- data[data$utla %in%
      counterfactual_areas, ]
  } else {
    counterfactual_agg <- data[data$utla %ni%
        affected_areas, ]
  }

  # Aggregates the counterfactual on date level
  counterfactual_agg <- counterfactual_agg %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), sum),
      .groups = "drop"
    ) %>%
    aggFunc(impact_measure) %>%
    select(date, starts_with(impact_measure)) %>%
    rename("All counterfactual areas" := impact_measure)

  # Joins on the affected and counterfactual area values
  data_comb_df <- data_comb_df %>%
    full_join(affected_agg, by = "date") %>%
    full_join(counterfactual_agg, by = "date")

  data_comb_df$date <- as.Date(data_comb_df$date, format = "%d/%m/%Y")

  
  return(data_comb_df)
}


#' `graph_arg_generation` This function takes the input data and then generates an aggregation function to produce the impact measure
#'
#' @param impact_measure: lfd_positivity, admissions_per_100k, deaths_per_100k

graph_arg_generation <- function(impact_measure) {

  # The aggFunc computes the respective impact measure selected above
  # The axisTitle is used in graphs later
  if (impact_measure == "lfd_positivity") {
    aggFunc <- function(df, impact_measure) {
      df[[impact_measure]] <- df$total_lfd_positive_tests / (df$total_lfd_positive_tests + df$total_lfd_negative_tests) * 100

      return(df)
    }
  } else if (impact_measure == "admissions_per_100k") {
    aggFunc <- function(df, impact_measure) {
      df[[impact_measure]] <- df$new_admissions / df$population_size * 100000

      return(df)
    }
  } else if (impact_measure == "deaths_per_100k") {
    aggFunc <- function(df, impact_measure) {
      df[[impact_measure]] <- df$deaths_per_100k / df$population_size * 100000

      return(df)
    }
  } 
  return(aggFunc)
}

