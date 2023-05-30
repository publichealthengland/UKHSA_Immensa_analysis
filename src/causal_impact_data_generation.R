
#' causal_impact_data_generation
#' prepares a data set for the causal impact modelling and graphs
#' @param selected_areas areas where the key to the list is the affected area
# and the values are the counterfactuals, containing the data frame with the following columns:
#'    - Affected.areas       
#'    - Counterfactual.areas
#' @param data the raw datasets that have been grouped by utla_code, utla, region and date
#' @param impact_measure lfd_positivity, admissions_per_100k, 7day_admissions_per_100k, deaths_per_100k, 7day_deaths_per_100k
#' @param data_comb_df data frame with columns for:
#'    - date, 
#'    - each of 9 areas of interest and 
#'    - their CF values
#' @param reporting_lag_start as.Date("2021-09-02")
#' @param individual_series  init_vars$individual_series for the production of tables
#' @param apply_log a column name from the init_vars for the production of tables
#' @param log_const a column name from the init_vars for the production of tables
#' @param rest_of_england all areas in England that are not covered by 9 areas of interest


causal_impact_data_generation <- function(selected_areas, data, impact_measure,
                                          data_comb_df, reporting_lag_start, individual_series,
                                          apply_log, log_const, rest_of_england) {
  if (rest_of_england == FALSE) {
    # Gets the unique counterfactual areas as a list for the following print
    # get all unique counterfactual areas if ROE is false
    counterfactual_areas <- unique(str_split(
      toString(selected_areas[["Counterfactual.areas"]]),
      ", "
    )[[1]])
  } else {
    # filter out all affected areas from the dataframe then get all unique utla's
    counterfactual_areas <- unique(data[data$utla %ni% selected_areas$Affected.areas, ][["utla"]])
  }

  # run through this section !!!!!!!!!!!
  if (individual_series == TRUE) {
    # Generates a data frame consisting of each counterfactual area
    # and their impact measure
    data <- data[data$utla %in% counterfactual_areas, ] %>%
      select(utla, date, impact_measure) %>%
      pivot_wider(
        names_from = "utla",
        values_from = impact_measure
      ) %>%
      cbind(data_comb_df["All affected areas"]) %>%
      tibble() %>%
      select("date", sort(colnames(.))) %>%
      select("date", "All affected areas", everything()) %>%
      as.data.frame()
    
  } else {
    data <- data_comb_df %>%
      select(
        date, `All affected areas`,
        `All counterfactual areas`
      )
  }
  
  data$date <- as.Date(data$date, format = "%d/%m/%Y")

  # Prepares the data for conducting Causal Impact calculations
  ci_data <- causalimpactprep(
    data,
    reporting_lag_start,
    apply_log,
    log_const
  )
  # Conducts the Causal Impact modelling
  ci <- CausalImpact(
    ci_data$time_df,
    ci_data$preperiod,
    ci_data$postperiod,
    model.args = list(niter = 10000)
  )
  # get data out of ci
  ci_new <- as.data.frame(ci$series) %>%
    cbind(date = rownames(.), .)
  
  data_new <- as.data.frame(ci_data$time_df) %>%
    cbind(date = rownames(.), .)
  # Adds the y-label (adds log if log applied)
  # this is where we would back transform if we wanted to convert it back
  y_lab <- impact_measure
  if (apply_log == TRUE) {
    y_lab <- glue("log({log_const} + {impact_measure})")
  }

  output <- list(ci, ci_data, ci_new, data_new, y_lab)

  names(output) <- c("ci", "ci_data", "ci_new", "data_new", "y_lab")

  return(output)
}

causalimpactprep <-
  function(df, reporting_lag_start, apply_log, log_const) {
    dates <- df$date
    # convert date to index
    if (apply_log == TRUE) {
      time_df <- zoo(
        df %>%
          select(-date) %>%
          lapply(adjusted_log, log_const) %>%
          as.data.frame(),
        dates
      )
    } else {
      time_df <- zoo(
        df %>%
          select(-date),
        dates
      )
    }

    preperiod <- c(min(dates), reporting_lag_start - 1)
    postperiod <- c(reporting_lag_start, max(dates))

    return(list(
      "time_df" = time_df,
      "preperiod" = preperiod,
      "postperiod" = postperiod
    ))
  }
