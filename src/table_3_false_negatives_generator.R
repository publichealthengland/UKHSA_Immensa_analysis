#' prop_test_lower_bound
#'
#' `prop_test_lower_bound()` returns the lower bound confidence interval from prop.test
#'
#' @param count_negative_pcr_tests_lab_x: int
#'
#' @param count_negative_pcr_tests_rest_of_england: int
#' 
#' @param count_positive_pcr_tests_lab_x: int
#' 
#' @param count_positive_pcr_tests_rest_of_england: int

prop_test_lower_bound <- function(count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
                                  count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england) {
  lower_bound <- tryCatch(
    {
      prop_matrix <- matrix(c(
        count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
        count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england
      ),
      ncol = 2
      )
      return(prop.test(prop_matrix)$conf.int[1])
    },
    error = function(cond) {
      return(0)
    }
  )
  return(lower_bound)
}

#' prop_test_upper_bound
#'
#' `prop_test_upper_bound()` returns the upper bound confidence interval from prop.test
#'
#' @param count_negative_pcr_tests_lab_x: int
#'
#' @param count_negative_pcr_tests_rest_of_england: int
#' 
#' @param count_positive_pcr_tests_lab_x: int
#' 
#' @param count_positive_pcr_tests_rest_of_england: int

prop_test_upper_bound <- function(count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
                                  count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england) {
  upper_bound <- tryCatch(
    {
      prop_matrix <- matrix(c(
        count_negative_pcr_tests_lab_x, count_negative_pcr_tests_rest_of_england,
        count_positive_pcr_tests_lab_x, count_positive_pcr_tests_rest_of_england
      ),
      ncol = 2
      )
      return(prop.test(prop_matrix)$conf.int[2])
    },
    error = function(cond) {
      return(0)
    }
  )
  return(upper_bound)
}

#' sampler
#' 
#' `sampler` returns a number of simulated values from a normal distribution
#' 
#' @param ninter: int - number of simulations
#' 
#' @param mean: float - average
#'
#' @param sd: float - standard deviation 
#' 
sampler <- function(niter, mean, sd){
  rnorm(niter, mean = mean, sd = sd)
}

#' lab_x_positive_test_calc
#'
#' `lab_x_positive_test_calc()` returns a string of the difference between expected positive tests and observed for a given set of variables
#'
#' @param df: data.frame
#'
#' @param variables_list: list

lab_x_positive_test_calc <- function(df, variables_list) {
  groupby_list <- append(variables_list, c(
    "count_pcr_tests_lab_x", "count_negative_pcr_tests_lab_x",
    "count_positive_pcr_tests_lab_x", "count_positive_pcr_tests_rest_of_england",
    "count_negative_pcr_tests_rest_of_england"
  ))

  df_agg <- df %>%
    select(all_of(groupby_list)) %>%
    group_by(across(all_of(variables_list))) %>%
    summarise(across(everything(), sum))
  
  df_agg$rest_of_england_tests <- (df_agg$count_positive_pcr_tests_rest_of_england + df_agg$count_negative_pcr_tests_rest_of_england)
  
  df_agg$positivity_rest_of_england_labs <- df_agg$count_positive_pcr_tests_rest_of_england / df_agg$rest_of_england_tests
  # Positivity for rest of england * Total tests for lab_x in group
  df_agg$expected_positive_tests_lab_x <- df_agg$positivity_rest_of_england_labs * (df_agg$count_negative_pcr_tests_lab_x +
    df_agg$count_positive_pcr_tests_lab_x)

  # Difference between expected positive tests and observed
  df_agg$lab_x_test_difference <- df_agg$expected_positive_tests_lab_x - df_agg$count_positive_pcr_tests_lab_x
  df_agg$lab_x_test_difference_lower_bound <- suppressWarnings(mapply(
    prop_test_lower_bound, df_agg$count_negative_pcr_tests_lab_x,
    df_agg$count_negative_pcr_tests_rest_of_england,
    df_agg$count_positive_pcr_tests_lab_x,
    df_agg$count_positive_pcr_tests_rest_of_england
  ))

  df_agg$lab_x_test_difference_upper_bound <- suppressWarnings(mapply(
    prop_test_upper_bound, df_agg$count_negative_pcr_tests_lab_x,
    df_agg$count_negative_pcr_tests_rest_of_england,
    df_agg$count_positive_pcr_tests_lab_x,
    df_agg$count_positive_pcr_tests_rest_of_england
  ))

  df_agg$lab_x_test_difference_lower_bound <- df_agg$lab_x_test_difference_lower_bound * (df_agg$count_negative_pcr_tests_lab_x +
    df_agg$count_positive_pcr_tests_lab_x)

  df_agg$lab_x_test_difference_upper_bound <- df_agg$lab_x_test_difference_upper_bound * (df_agg$count_negative_pcr_tests_lab_x +
    df_agg$count_positive_pcr_tests_lab_x)
 
  # obtain midpoint for the normal distribution
  df_agg$midpoint <- (df_agg$lab_x_test_difference_lower_bound + df_agg$lab_x_test_difference_upper_bound) / 2
  # obtain standard dev from 95%CI values
  df_agg$sd <- abs(df_agg$lab_x_test_difference_lower_bound - df_agg$lab_x_test_difference_upper_bound) / 3.96 
  nsim <- 1000
  set.seed(42)
  lower_ci <- floor(quantile(rowSums(data.frame(mapply(sampler, nsim, df_agg$midpoint, df_agg$sd))), 0.025, na.rm = T))
  upper_ci <- ceiling(quantile(rowSums(data.frame(mapply(sampler, nsim, df_agg$midpoint, df_agg$sd))), c(0.9725), na.rm = T))
  
  return(paste0(
    comma_format()(ceiling(sum(df_agg$lab_x_test_difference, na.rm = T))),
    " [", comma_format()(lower_ci),
    " - ", comma_format()(upper_ci),
    "]"
  ))
}

#' unadjust_estimates_of_false_negatives
#'
#' `unadjust_estimates_of_false_negatives()` returns a string of the difference between expected positive tests and observed
#'
#' @param df: data.frame

unadjust_estimates_of_false_negatives <- function(df) {
  positivity_rest_of_england_labs <- (sum(df$count_positive_pcr_tests_rest_of_england) / (sum(df$count_positive_pcr_tests_rest_of_england) + sum(df$count_negative_pcr_tests_rest_of_england)))
  expected_positive_tests_lab_x <- positivity_rest_of_england_labs * (sum(df$count_positive_pcr_tests_lab_x) + sum(df$count_negative_pcr_tests_lab_x))
  lab_x_test_difference <- expected_positive_tests_lab_x - sum(df$count_positive_pcr_tests_lab_x)
  prop_test_matrix <- matrix(c(
    sum(df$count_negative_pcr_tests_lab_x), sum(df$count_negative_pcr_tests_rest_of_england),
    sum(df$count_positive_pcr_tests_lab_x), sum(df$count_positive_pcr_tests_rest_of_england)
  ),
  ncol = 2
  )

  prop_test_res <- prop.test(prop_test_matrix)

  lower_bound <- prop_test_res$conf.int[1] * (sum(df$count_positive_pcr_tests_lab_x) + sum(df$count_negative_pcr_tests_lab_x))
  upper_bound <- prop_test_res$conf.int[2] * (sum(df$count_positive_pcr_tests_lab_x) + sum(df$count_negative_pcr_tests_lab_x))
  
  return(paste0(
    comma_format()(ceiling(lab_x_test_difference)), " [",
    comma_format()(ceiling(lower_bound)), " - ",
    comma_format()(ceiling(upper_bound)), "]"
  ))
}

#' produce_false_negative_table
#'
#' `produce_false_negative_table()` Produces the estimated false negative tests table
#'
#' @param df: data.frame - PCR Data stored in `initialised_data$df_pcr`

table_3_produce_false_negative_table <- function(df) {
  df$specimen_processed_date <- as.Date(df$specimen_processed_date)
  df <- df %>% filter((specimen_processed_date >= "2021-09-02") &
    (specimen_processed_date <= "2021-10-12"))

  # Unadjusted false negatives
  unadjusted_values <- unadjust_estimates_of_false_negatives(df)

  # 2. Adjusting for channel split
  channels <- lab_x_positive_test_calc(df, c("test_site"))

  # 3. Adjusting for channels and age

  channels_age <- lab_x_positive_test_calc(df, c("age_group", "test_site"))

  # 4. Adjusting for channels, age and region

  channels_age_region <- lab_x_positive_test_calc(df, c("region", "age_group", "test_site"))

  # 5. Adjusting for channels, age and time

  channels_age_ltla <- lab_x_positive_test_calc(df, c("ltla", "age_group", "test_site"))

  # 6. Adjusting for channels, age, region and time

  channels_age_region_time <- lab_x_positive_test_calc(df, c("specimen_processed_date", "region", "age_group", "test_site"))
  
  figure <- data.frame(matrix(append(
    c(
      "Unadjusted", "Adjusting for test site", "Adjusting for age and test site",
      "Adjusting for age, region and test site", "Adjusting for age, region, test site and date"
    ),
    c(
      unadjusted_values, channels, channels_age, channels_age_region,
      channels_age_region_time
    )
  ), ncol = 2))
  colnames(figure) <- c("Methodology", "Estimate of incorrect negatives for England (95% CI)")
  return(figure)
}
