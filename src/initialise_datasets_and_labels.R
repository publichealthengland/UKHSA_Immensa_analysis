#' initialise_raw_data_sets
#'
#' `initialise_raw_data_sets()` Loads the raw datasets
#'
#' @param raw_data_file: str - Location of Summary Lab X Data
#'
#' @param lfd_file: str - Location of Lateral Flow Test Data
#' 
#' @param pcr_file: str - Location of Polymerase Chain Reaction Data
#' 
#' @param surrounding_areas: bool - get a list of all areas for the affected to counterfactual map

initialise_raw_data_sets <- function(raw_data_file, lfd_file, pcr_file, surrounding_areas) {
  # Load the data and sum for each date
  init_data <- read_csv(paste0("./data/", raw_data_file))
  df_lfd <- read_csv(paste0("./data/", lfd_file))
  df_pcr <- read_csv(paste0("./data/", pcr_file))
  
  # convert date columns to date time
  init_data$date <- as.Date(init_data$date, format = '%d/%m/%Y')
  df_lfd$test_date <- as.Date(df_lfd$test_date, format = '%d/%m/%Y')
  df_pcr$specimen_processed_date <- as.Date(df_pcr$specimen_processed_date, format = '%d/%m/%Y')
  
  # sums total positive and negative lfd tests by day by ltla
  # removes the age breakdown
  df_lfd_ltla <- df_lfd %>% group_by(test_date,ltla_code) %>% 
    summarise(total_lfd_positive_tests = sum(total_positive_tests),
              total_lfd_negative_tests = sum(total_negative_tests))
  
  # gets lfd positivity per day per ltla
  # df_lfd_ltla$total_lfd_positivity <- (df_lfd_ltla$total_lfd_positive_tests / 
  #                                        df_lfd_ltla$total_lfd_negative_tests) * 100
  
  # left join aggregated lfd data to summary dataset
  init_data <- init_data %>% left_join(df_lfd_ltla, by = c('date' = 'test_date', 
                                                           'ltla_code' = 'ltla_code'))
  # group summary data by utla, summing all numeric values
  data <- init_data %>%
    group_by(utla_code, utla, region, date) %>%
    summarise(across(where(is.numeric), sum), .groups = "drop")

  # Compute the different impact measures
  # lfd positivity
  data$lfd_positivity <- data$total_lfd_positive_tests / 
    (data$total_lfd_positive_tests + data$total_lfd_negative_tests) * 100
  
  # admissions per 100k
  data$admissions_per_100k <- (data$new_admissions / data$population_size) * 100000
  
  # deaths per 100k
  data$deaths_per_100k <- (data$new_deaths_28_days / data$population_size) * 100000
  
  # The UTLA names containing commas are replaced with semi-colons
  # to facilitate their selection by R (the string is split on comma)
  # Example name is "Bristol, City of"
  data$utla <- str_replace_all(data$utla, ",", ";")

  # Fill all NA values with 0
  data[is.na(data)] <- 0

  df_bubble <- read_pct_data_utla(df_pcr)

  df_geospatial_map <- load_counter_factual_map_data(shape_file, surrounding_areas)

  ltla_lfd_daily_growth_rate_percent <- read_pct_data_lfd_ltla(df_pcr, df_lfd)
  
  return(list(
    "init_data" = init_data, 
    "df_lfd" = df_lfd,
    "df_pcr" = df_pcr, 
    "data" = data, 
    "df_bubble" = df_bubble,
    "df_geospatial_map" = df_geospatial_map,
    "ltla_lfd_daily_growth_rate_percent" = ltla_lfd_daily_growth_rate_percent
  ))
}

#' initialise_selected_areas_and_labels
#'
#' `initialise_selected_areas_and_labels()` Loads the selected areas for the given area
#'
#' @param prop_tests_in_area: list, proportion of tests undertaken by the top 9 UTLAs
#'
#' @param prop_tests_lab_x: proportion of tests undertaken by Lab x
#' 
#' @param areas: List of the affected areas and their counterfactual areas
#' 
#' @param rest_of_england: bool

initialise_selected_areas_and_labels <- function(prop_tests_in_area,
                                                 prop_tests_lab_x,
                                                 areas,
                                                 rest_of_england) {

  # The list of areas where the key to the list is the affected area
  # and the values are the counterfactuals

  selected_areas <- data.frame(
    `Affected areas` = character(0),
    `Counterfactual areas` = character(0)
  )

  # Adding the counterfactuals and affected areas
  # to a single data_comb_df
  i <- 1
  Affected_area <- c("All affected areas")
  for (name in (names(areas))) {
    selected_areas[i, ] <- c(name, toString(areas[[name]]))
    i <- i + 1
    Affected_area <- append(Affected_area, name)
  }

  # The proportion of tests conducted by lab_x during the affected
  # time period
  labels <- as.data.frame(do.call(cbind, list(
    Affected_area = Affected_area,
    prop_tests_in_area = prop_tests_in_area,
    prop_tests_lab_x = prop_tests_lab_x
  )))

  # Remove counterfactuals for this case
  if (rest_of_england == TRUE) {
    selected_areas <- selected_areas %>% select(Affected.areas)
  }

  labels["label"] <- paste0(
    labels$Affected_area,
    " (",
    as.character(labels$prop_tests_in_area),
    "%, ",
    as.character(labels$prop_tests_lab_x), "%)"
  )

  return(list("selected_areas" = selected_areas, "labels" = labels))
}

#' load_counter_factual_map_data
#'
#' `load_counter_factual_map_data()` Create the labels for the affected areas and their respective counterfactuals
#'
#' @param shape_file: str - Location of the UTLA shape file
#'
#' @param surrounding_areas: bool - to obtain the list of areas for the map

load_counter_factual_map_data <- function(shape_file, surrounding_areas) {
  areas <- initialise_areas(FALSE, FALSE)

  affected_area <- names(areas)
  counterfactual <- unique(unlist(areas))

  affected_nm_to_label <- list(
    "Bath and North East Somerset" = "5",
    "Bristol, City of" = "3",
    "North Somerset" = "4",
    "South Gloucestershire" = "2",
    "Swindon" = "1",
    "West Berkshire" = "6",
    "Wiltshire" = "9",
    "Gloucestershire" = "7",
    "Somerset" = "8"
  )
  affected_to_label <- list(
    "E06000022" = "5",
    "E06000023" = "3",
    "E06000024" = "4",
    "E06000025" = "2",
    "E06000030" = "1",
    "E06000037" = "6",
    "E06000054" = "9",
    "E10000013" = "7",
    "E10000027" = "8"
  )

  df_shape <- load_shape_data()
  rest_of_england_map_df <- df_shape %>% filter((utla_20_nm %ni% affected_area) & (utla_20_nm %ni% counterfactual))
  affected_df <- df_shape %>% filter(utla_20_nm %in% affected_area)
  counterfactual_df <- df_shape %>% filter(utla_20_nm %in% counterfactual)
  t <- list()
  for (i in names(areas)) t[[i]] <- as.data.frame(list(replicate(5, i), areas[[i]]), col.names = c("affected", "cf"))
  cf_af <- do.call(rbind, t)
  rownames(cf_af) <- NULL
  affected_mapping <- as.data.frame(t(as.data.frame(affected_to_label)))
  affected_mapping$utla_20_cd <- row.names(affected_mapping)
  row.names(affected_mapping) <- NULL
  names(affected_mapping) <- c("rank", "utla_20_cd")
  affected_df <- affected_df %>% left_join(affected_mapping)
  cf_af <- select(cf_af %>% left_join(affected_df[, c("utla_20_nm", "rank")], by = c("affected" = "utla_20_nm")), c("affected", "cf", "rank"))
  counter_factual_to_affected <- aggregate(
    data = cf_af,
    rank ~ cf,
    function(x) unique(x)
  )
  counter_factual_to_affected$rank <- gsub('\\c|\\"|\\(|\\)', "", as.character(counter_factual_to_affected$rank))
  counterfactual_df <- counterfactual_df %>% left_join(counter_factual_to_affected, by = c("utla_20_nm" = "cf"))
  return(list("rest_of_england_map_df" = rest_of_england_map_df, "affected_df" = affected_df, "counterfactual_df" = counterfactual_df))
}


#' load_shape_data
#'
#' `load_shape_data()` returns a shape file of each UTLA within England with the centroid calculated
#' dependancies - sf
#'
#' @param file_path: str - Location of the UTLA shape file

load_shape_data <- function() {
  print("Loading Shape Data")
  # Source: Office for National Statistics licensed under the Open Government Licence v.3.0
  # Contains OS data © Crown copyright and database right 2021
  df_shape <- st_read("./data/Counties_and_Unitary_Authorities_eng.geojson")
  
  # rename columns
  names(df_shape) <- c("utla_20_cd", "utla_20_nm", "geometry")
  # obtain the centroid of each UTLA
  df_shape$centroid <- st_centroid(x = df_shape$geometry) %>% st_geometry()
  
  return(df_shape)
}
#' read_pct_data_utla
#'
#' `read_pct_data_utla()` returns a data.frame of the percentage of PCR tests undertaken by Laboratory X
#' dependancies - sf
#'
#' @param pct_file_path: data.frame - pcr data
#'
read_pct_data_utla <- function(df_pct) {
  # convert to a date object
  
  df_pct <- df_pct %>%
    filter((specimen_processed_date >= "2021-09-02") &
      (specimen_processed_date <= "2021-10-12")) %>%
    select(c(
      "utla", "utla_code",
      "count_pcr_tests_lab_x", "count_pcr_tests_rest_of_england"
    ))

  df_pct <- df_pct %>%
    group_by(utla, utla_code) %>%
    summarise(
      count_pcr_tests_lab_x = sum(count_pcr_tests_lab_x),
      count_pcr_tests_rest_of_england = sum(count_pcr_tests_rest_of_england)
    )

  df_pct$percentage_tests_lab <- (df_pct$count_pcr_tests_lab_x / (
    df_pct$count_pcr_tests_lab_x + df_pct$count_pcr_tests_rest_of_england)) * 100

  df_shape <- load_shape_data()

  df_merged <- df_shape %>% left_join(df_pct, by = c("utla_20_cd" = "utla_code"))
  # nudge the centroid of Leicestershire as the centroid overlaps with Leicester
  df_merged[df_merged$utla_20_nm == "Leicestershire", "centroid"] <- df_merged[df_merged$utla_20_nm == "Leicestershire", "centroid"] + 0.05#4250
  return(df_merged)
}


#################
# For Growth Rate graph - data processing
################

read_pct_data_lfd_ltla <- function(df_pcr, df_lfd) {
  # convert to a date object
  
  date_diff <- as.numeric(as.Date("2021-10-24") - as.Date("2021-08-17"))
  
  df_pcr$specimen_processed_date <- as.Date(df_pcr$specimen_processed_date)
  # ltla to region code mapping as the lfd dataset does not contain this info
  df_ltla_to_region <- df_pcr %>% select(c('ltla','ltla_code','region')) %>%
    distinct(ltla,ltla_code,region, .keep_all = TRUE)
  
  # obtain the percentage of tests undertaken by Lab X for each ltla
  df_pct <- df_pcr %>%
    filter((specimen_processed_date >= "2021-09-02") &
             (specimen_processed_date <= "2021-10-12")) %>%
    select(c(
      "ltla", "ltla_code",
      "count_pcr_tests_lab_x", "count_pcr_tests_rest_of_england"
    ))
  
  df_pct <- df_pct %>%
    group_by(ltla, ltla_code) %>%
    summarise(
      count_pcr_tests_lab_x = sum(count_pcr_tests_lab_x),
      count_pcr_tests_rest_of_england = sum(count_pcr_tests_rest_of_england)
    )
  
  df_pct$percentage_tests_lab <- (df_pct$count_pcr_tests_lab_x / (
    df_pct$count_pcr_tests_lab_x + df_pct$count_pcr_tests_rest_of_england)) * 100
  
  df_pct <- df_pct %>% left_join(df_ltla_to_region, by = c("ltla"="ltla"))
  
  # process lfd data to get the growth rate between 17th Aug to 24th Oct

  df_lfd_raw_num_aug <- df_lfd %>%
    filter((test_date >= "2021-08-17") &
             (test_date <= "2021-08-23")) %>% 
    group_by(ltla) %>% 
    summarise(total_positive_tests = sum(total_positive_tests),
              total_negative_tests= sum(total_negative_tests)) 
  
  df_lfd_raw_num_aug$date <- 'aug'
  
  df_lfd_raw_num_oct <- df_lfd %>%
    filter((test_date >= "2021-10-18") &
             (test_date <= "2021-10-24")) %>%
    group_by(ltla) %>% 
    summarise(total_positive_tests = sum(total_positive_tests),
              total_negative_tests= sum(total_negative_tests)) 
  
  df_lfd_raw_num_oct$date <- 'oct'
  
  df_lfd_joined <- rbind(df_lfd_raw_num_aug, df_lfd_raw_num_oct)
  
  df_lfd_joined$positivity <- df_lfd_joined$total_positive_tests / (
    df_lfd_joined$total_positive_tests+df_lfd_joined$total_negative_tests) * 100
  
  
  df_lfd_growth_rate <- df_lfd_joined %>% 
    group_by(ltla) %>% 
    mutate(daily_growth_rate = ((((positivity/lag(positivity, 1))**(1/date_diff))-1)*100)) %>%
    filter(!is.na(daily_growth_rate)) %>% select(c("ltla","daily_growth_rate")) %>% 
    left_join(df_pct, by = c("ltla"="ltla"))
  
  return(df_lfd_growth_rate)
}
