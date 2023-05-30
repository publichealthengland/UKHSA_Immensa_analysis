#' get_LTLA_prop_tests_lab_x
#'
#' `get_LTLA_prop_tests_lab_x()` returns a data-frame of percentage tests sent to the lab_x lab, by LTLA, over the specified period.
#'
#' dependencies - dplyr, uses pipes, %>%
#'
#' @param df data frame with columns for:
#' specimen_processed_date, count_pcr_tests_lab_x & count_pcr_tests_rest_of_england
#' @param incident_period a vector of dates which is used to filter data to.
#' @param perc_cutpoints is a vector of numbers between 0 and 100, used to cut the outcome into categories.
#' @return dataframe containing three columns: 
#' - LTLA code, 
#' - proportion of tests (e.g. 0.8), 
#' - category of proportion of tests (e.g. 0-5%).
#'

get_LTLA_prop_tests_lab_x <- function(df,
                                      incident_period,
                                      perc_cutpoints) {
  df_out <- df %>%
    # ensure data in date format
    mutate(specimen_processed_date = as.Date(specimen_processed_date)) %>%
    group_by(ltla_code) %>%
    # limit to incident period
    filter(specimen_processed_date %in% incident_period) %>%
    # calculate the proportion of tests over the period that were undertaken at lab_x
    summarise(prop_tests_lab_x = sum(count_pcr_tests_lab_x) / sum(count_pcr_tests_lab_x + count_pcr_tests_rest_of_england) * 100) %>%
    # break down into categories ... based on inputs
    mutate(lab_x_cat = cut(prop_tests_lab_x, perc_cutpoints))

  return(df_out)
}

#' `get_cleaned_lfd_positivity()` returns a dataframe with several columns:
#'   - ltla_code, 
#'   - date, 
#'   - region, 
#'   - positivity, 
#'   - rolling_positivity
#'
#' @param df with ltla_code, date, region & total positive and negative lfd tests.
#'
get_cleaned_lfd_positivity <- function(df) {
  df_out <- df %>%
    # set date to date object
    mutate(date = as.Date(format(as.Date(test_date, format = "%d/%m/%Y"), "%Y-%m-%d"))) %>%
    group_by(ltla_code, date, region) %>%
    # calculate positivity for a given day (all ages together)
    summarise(
      total_positive_tests = sum(total_positive_tests),
      total_negative_tests = sum(total_negative_tests)
    ) %>%
    mutate(positivity = total_positive_tests / (total_negative_tests + total_positive_tests)) %>%
    group_by(ltla_code) %>%
    # order in date order for the rolling calculation
    arrange(date) %>%
    # calculate rolling positivity over 7 days with central alignment
    mutate(
      rolling_sum_tests = zoo::rollsum(x = (total_negative_tests + total_positive_tests), k = 7, fill = NA, align = "center"),
      rolling_sum_positives = zoo::rollsum(x = total_positive_tests, k = 7, fill = NA, align = "center"),
      rolling_positivity = rolling_sum_positives / rolling_sum_tests
    ) %>%
    select(ltla_code, date, region, positivity, rolling_positivity)

  return(df_out)
}

#' `get_LTLA_quantiles_SD()` returns a data-frame including IQR and median rolling sum of deaths & hospitalisations per 100k in the lab_x category.
#' dependancies - dplyr, uses pipes, %>%
#'
#' @param df data frame with columns for:
#' -`7day_deaths_per_100k`, 
#' -`7day_admissions_per_100k`, 
#' - `date` 
#' - `lab_x_cat`
#'
get_LTLA_quantiles_SD <- function(df) {
  df_out <- df %>%
    group_by(date, lab_x_cat) %>%
    summarize(
      rolling_sumDeaths_per_100k_50 = quantile(na.rm = T, `7day_deaths_per_100k`, prob = .5),
      rolling_sumDeaths_per_100k_25 = quantile(na.rm = T, `7day_deaths_per_100k`, prob = .25),
      rolling_sumDeaths_per_100k_75 = quantile(na.rm = T, `7day_deaths_per_100k`, prob = .75),
      rolling_sumHosp_per_100k_50 = quantile(na.rm = T, `7day_admissions_per_100k`, prob = .5),
      rolling_sumHosp_per_100k_25 = quantile(na.rm = T, `7day_admissions_per_100k`, prob = .25),
      rolling_sumHosp_per_100k_75 = quantile(na.rm = T, `7day_admissions_per_100k`, prob = .75)
    ) %>%
    ungroup()

  return(df_out)
}

#' `get_LTLA_quantiles()` returns a data-frame including IQR and median rolling sum of deaths & hospitalisations per 100k in the lab_x category.
#' dependancies - dplyr, uses pipes, %>%
#'
#' @param df data frame with columns for:
#' `7day_deaths_per_100k`, `7day_admissions_per_100k`, date & lab_x_cat
#'
get_LTLA_quantiles_Positivity <- function(df) {
  df_out <- df %>%
    group_by(date, lab_x_cat) %>%
    summarize(
      lab_x_positivity_50 = quantile(na.rm = T, `rolling_positivity`, prob = .5) ,
      lab_x_positivity_25 = quantile(na.rm = T, `rolling_positivity`, prob = .25),
      lab_x_positivity_75 = quantile(na.rm = T, `rolling_positivity`, prob = .75)
    ) %>%
    ungroup() 

  return(df_out)
}

#' `get_lab_xprop_labels()` returns a vector with the label names for each lab_x category
#' dependancies - dplyr
#'
#' @param df subsetted regional data-frame to be plotted, with the following variables;
#' date, lab_x_cat, ltla_code
#'
get_lab_xprop_labels <- function(df) {
  temp <- df %>%
    group_by(lab_x_cat) %>%
    summarise(n_ltla = length(unique(ltla_code))) %>%
    as.data.frame()

  prop_labels <- paste0(
    gsub(gsub(temp$lab_x_cat,
      pattern = c("]"),
      replacement = c("%)")
    ),
    pattern = c(","),
    replacement = c("-")
    ), ", N = ",
    temp$n_ltla
  )

  names(prop_labels) <- temp$lab_x_cat

  return(prop_labels)
}

#' `get_data_ready_for_plots` returns a list of df_sd_outcomes, df_prop_lab_x and df_lfd_plot
#'
#' @param df_lfd data.frame - Lateral Flow Testing Data, `initialised_data$df_lfd` 
#' @param df_pcr data.frame - Polymerase Chain Reaction Testing Data `initialised_data$df_pcr` 
#' @param df_sd_outcomes data.frame for plotting LTLA daily deaths - `plot_data$df_sd_outcomes`
#' @param reporting_lag_start When the reporting lag started i.e. as.Date("2021-09-02")
#' @param reporting_lag_end When the reporting lag ended i.e. as.Date("2021-10-12")

get_data_ready_for_plots <- function(df_lfd, df_pcr, df_sd_outcomes, reporting_lag_start, reporting_lag_end) {
  # read from csv to data-frame
  df_sd_outcomes$date <- as.Date(df_sd_outcomes$date, "%d/%m/%Y")
  df_prop_lab_x <- get_LTLA_prop_tests_lab_x(
    df = df_pcr,
    incident_period = seq(reporting_lag_start, reporting_lag_end, by = "days"),
    perc_cutpoints = c(0, 5, 20, 100)
  )
  df_lfd_plot <- get_cleaned_lfd_positivity(df_lfd)

  return(list("df_sd_outcomes" = df_sd_outcomes, "df_prop_lab_x" = df_prop_lab_x, "df_lfd_plot" = df_lfd_plot))
}
