#' input_variables
#'
#' `input_variables()` Initialise the starting variables for the analysis, setting up the reporting lag and start period, impact period and the comparator periods for causal impact analysis
#'

input_variables <- function() {

  # Switching variables
  # Dates for analysis
  reporting_lag_start <- as.Date("2021-09-02")
  reporting_lag_end <- as.Date("2021-10-12")
  impact_period_start <- as.Date("2021-10-18")
  impact_period_end <- as.Date("2021-10-24")
  comparator_period_start <- as.Date("2021-08-26")
  comparator_period_end <- as.Date("2021-09-01")

  # Parameters to decide the number of neighbours,
  # and whether it is rest of England or nearest neighbours
  # or whether we are considering surrounding areas only
  rest_of_england <- FALSE
  surrounding_areas <- FALSE
  knn_neighbours <- 5

  # The end date argument determines where the series has converged
  # in the counterfactual vs affected area series
  # A time lag of 1 week has been set between LFD positivity and
  # hospitalisation and a further week between hospitalisation and deaths

  impact_measure_with_end_date <- list(
    lfd_positivity = as.Date("2021-10-31"),
    admissions_per_100k = as.Date("2021-11-14"),
    deaths_per_100k = as.Date("2021-11-28")
  )

  # Coefficient for creating summary data
  coeff_lfdpos_caserates <- 20.0

  # Use log transformed series as input for causal modelling
  apply_log <- TRUE
  log_const <- 1
  
  revert_log <- TRUE

  # Use individual counterfactual series as input for causal modelling
  individual_series <- TRUE

  prop_tests_in_area <- c(
    34.4, 41.9, 40.6, 40.5, 37.9, 36.2,
    34.1, 30.6, 26.1, 21.1
  )

  prop_tests_lab_x <- c(
    55.6, 5.6, 5.7, 8.7, 4.5, 3.9,
    2.7, 9.7, 8.7, 6.2
  )

  affected_areas_colour <- "#d62728"
  counterfactual_areas_colours <- "blue"
  synthetic_counterfactual_colour <- "green"

  ci_start <- "2021-09-02"

  use_common_legend <- TRUE

  return(list(
    "reporting_lag_start" = reporting_lag_start, "reporting_lag_end" = reporting_lag_end,
    "impact_period_start" = impact_period_start, "impact_period_end" = impact_period_end,
    "comparator_period_start" = comparator_period_start, "comparator_period_end" = comparator_period_end,
    "rest_of_england" = rest_of_england, "surrounding_areas" = surrounding_areas,
    "knn_neighbours" = knn_neighbours, "apply_log" = apply_log,
    "log_const" = log_const, "individual_series" = individual_series,
    "prop_tests_in_area" = prop_tests_in_area, "prop_tests_lab_x" = prop_tests_lab_x,
    "affected_areas_colour" = affected_areas_colour, "counterfactual_areas_colours" = counterfactual_areas_colours,
    "synthetic_counterfactual_colour" = synthetic_counterfactual_colour, "ci_start" = ci_start,
    "impact_measure_with_end_date" = impact_measure_with_end_date, "use_common_legend" = use_common_legend,
    "revert_log" = revert_log ,
    "coeff_lfdpos_caserates" = coeff_lfdpos_caserates
  ))
}

#' initialise_areas
#'
#' `initialise_areas()` Initialise the Affected to counterfactual list for analysis
#'

initialise_areas <- function(surrounding_areas, replace_commas = TRUE) {
  # These are the list of counterfactuals for each given affected areas
  if (surrounding_areas == TRUE) {
    area_list <- c(
      "Dorset", "Devon", "Hampshire", "Oxfordshire",
      "Worcestershire", "Warwickshire",
      "Herefordshire, County of",
      "Reading", "Wokingham"
    )
    areas <- list(
      `Swindon` = area_list,
      `South Gloucestershire` = area_list,
      `Bristol; City of` = area_list,
      `North Somerset` = area_list,
      `Bath and North East Somerset` = area_list,
      `West Berkshire` = area_list,
      `Gloucestershire` = area_list,
      `Somerset` = area_list,
      `Wiltshire` = area_list
    )
  } else {
    # List of affected area bound to its counterfactual areas
    areas <-
      list(
        Swindon = c(
          "Telford and Wrekin", "Milton Keynes",
          "Central Bedfordshire", "Stockport", "Trafford"
        ),
        `South Gloucestershire` = c(
          "Windsor and Maidenhead", "Cheshire East",
          "Cheshire West and Chester", "Central Bedfordshire",
          "Bournemouth, Christchurch and Poole"
        ),
        `Bristol, City of` = c(
          "Nottingham", "Brighton and Hove",
          "Portsmouth", "Southampton", "Manchester"
        ),
        `North Somerset` = c(
          "Cheshire East", "Cheshire West and Chester",
          "Bournemouth, Christchurch and Poole", "Dorset", "Solihull"
        ),
        `Bath and North East Somerset` = c(
          "Brighton and Hove", "Cheshire West and Chester",
          "Central Bedfordshire", "Bournemouth, Christchurch and Poole",
          "Solihull"
        ),
        `West Berkshire` = c(
          "Bracknell Forest", "Windsor and Maidenhead",
          "Central Bedfordshire", "Solihull", "Havering"
        ),
        Gloucestershire = c(
          "Cambridgeshire", "Derbyshire",
          "Leicestershire", "Nottinghamshire", "Oxfordshire"
        ),
        Somerset = c(
          "Shropshire", "Cornwall",
          "Dorset", "Devon", "East Sussex"
        ),
        Wiltshire = c(
          "Shropshire", "Dorset",
          "Buckinghamshire", "Leicestershire", "Oxfordshire"
        )
      )
  }

  if (replace_commas == TRUE) {
    # This is required later as R does not allow list to be stored
    # in a dataframe, so the list is converted to a string and converted
    # back to a list based on comma separation
    names(areas) <- str_replace(names(areas), ", ", "; ")

    # Replaces the counterfactual comma names with semi-colons also
    areas <- lapply(areas, str_replace, ", ", "; ")
  }

  return(areas)
}
