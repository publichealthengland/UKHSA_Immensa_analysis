#' merge_initial_data_grid_fig
#'
#' `merge_initial_data_grid_fig()`  Merges the g1 and g2 graphs
#'
#' @param data_comb_df: data frame with columns for:
#'    - date, 
#'    - each of 9 areas of interest and 
#'    - their CF values
#' @param reporting_lag_start: When the reporting lag started i.e. as.Date("2021-09-02")
#' @param reporting_lag_end: When the reporting lag ended i.e. as.Date("2021-10-12") 
#' @param labels: List of the affected areas and the proportion of tests undertaken in the area 
#' @param prop_tests_in_area: List of the proportion of tests in the affected areas
#' @param prop_tests_lab_x: List of proportion of tests undertaken by the lab x 
#' @param prop_tests_in_area: List of proportion of tests in the area
#' @param prop_tests_lab_x:  List of proportion of tests in the lab x
#' @param affected_areas_colour: str - Colour to plot the affected areas
#' @param counterfactual_areas_colours: str - Colour to plot the coutnerfactual areas 
#' @param use_common_legend: select if you want a to use a common legend
#' @param impact_measure: lfd_positivity, admissions_per_100k, 7day_admissions_per_100k, deaths_per_100k, 7day_deaths_per_100k
#' @param grid_plot_height: int - height of saved plots 
#' @param grid_plot_width: int - width of saved plots
#' @param produce_raw_data_graphs: if TRUE raw data graphs are produced and saved in the path_to_save 

merge_initial_data_grid_fig <- function(data_comb_df, reporting_lag_start, reporting_lag_end, labels, prop_tests_in_area,
                                        prop_tests_lab_x, affected_areas_colour, counterfactual_areas_colours, use_common_legend,
                                        impact_measure,  grid_plot_height, grid_plot_width, produce_raw_data_graphs) {
  if (impact_measure == "lfd_positivity") {
    y_lab <- "LFD Positivity (%)\n"
  } else if (impact_measure == "admissions_per_100k") {
    y_lab <- "Admissions (per 100k)\n"
  } else if (impact_measure == "deaths_per_100k") {
    y_lab <- "Deaths (per 100k)\n"
  }

  # Figure 1 plotting all affected and all counterfactual areas
  g1 <- g1_function(
    data_comb_df, reporting_lag_start, reporting_lag_end, prop_tests_in_area,
    prop_tests_lab_x, affected_areas_colour, counterfactual_areas_colours, y_lab
  )
  g1_leg <- get_legend(g1)
  # plot 9 area specific graphs
  g2 <- plot_list_0_function(
    data_comb_df, reporting_lag_start, reporting_lag_end, labels,
    affected_areas_colour, counterfactual_areas_colours, y_lab
  )

  # Combining all graphs together and adding a single legend for all
  merged_plot <- ggarrange(g1, # First row with the Figure 1
    ggarrange(g2, ncol = 1), # Second row with the area plots
    nrow = 2, common.legend = use_common_legend, legend = "bottom"
  ) + theme(plot.margin = margin(0.1, 0.1, 2, 0.1, "cm"))

  if (produce_raw_data_graphs == TRUE) {
    path_to_save <- file.path("outputs","visuals", "raw_data")

    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = paste0(impact_measure, ".png"),
      plot = merged_plot,
      path = path_to_save,
      height = grid_plot_height,
      width = grid_plot_width
    )
  }
  return(merged_plot)
}


#' `g1_function()`  Creates and prints out Figure 1 plotting all affected and all counterfactual areas
#'
#' @param data_comb_df: data frame with columns for:
#' date, each of 9 areas on interest and their CF values
#'
#' @param reporting_lag_start: currently set as 2021-09-02
#'
#' @param reporting_lag_end: currently set as 2021-10-12
#'
#' @param prop_tests_in_area: proportion of PCR tests done
#'
#' @param prop_tests_lab_x: proportion of PCR tests done by lab_x
#'
#' @param affected_areas_colour: colour used for the Affected areas
#'
#' @param counterfactual_areas_colours: colour used for the Counterfactual areas
#' 
#' @param y_lab: y axis labels for the parameters covering the impact_measure

g1_function <- function(data_comb_df, reporting_lag_start, reporting_lag_end, prop_tests_in_area,
                        prop_tests_lab_x, affected_areas_colour, counterfactual_areas_colours, y_lab) {
  
  max_y <- max(data_comb_df$`All affected areas`) * 0.9
  
  g1 <- ggplot(data = data_comb_df, aes(y = `All counterfactual areas`, x = date)) +
    geom_line(aes(color = "Comparator areas"), linetype = "dashed", size = 1) +
    geom_line(aes(y = `All affected areas`, color = "Affected areas"), linetype = "solid", size = 1.2) +
    scale_color_manual(
      name = "",
      values = c(
        "Affected areas" = affected_areas_colour,
        "Comparator areas" = counterfactual_areas_colours
      )
    ) +
    geom_vline(xintercept = reporting_lag_start, linetype = "dashed") +
    annotate(
      geom = "text",
      x = mean(c(reporting_lag_start, reporting_lag_end)),
      y = max_y, # need to make chanegable
      label = "Period the\nLaboratory\nwas operational", size = 6
    ) +
    geom_vline(xintercept = reporting_lag_end, linetype = "dashed") +
    theme_minimal() + # base_family = "Calibri"
    theme(axis.title.x = element_blank()) +
    # changing the date labeling on x axis
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    theme(axis.line.x.bottom = element_line(color = "grey 37")) +
    # Colouring % of prop_tests_in_area in green
    ggtitle(paste0(
      "A: Top nine affected UTLAs combined <br>(",
      "<span style='color:#009E73;'>", prop_tests_in_area[[1]], "%</span>",
      " of tests in UTLAs associated with the Laboratory,<br/> accounting for ",
      prop_tests_lab_x[[1]], " per cent of all the Laboratory tests)"
    )) +
    theme(plot.title = element_text(hjust = 0.5, size = 16)) +
    ylab(y_lab) +
    theme(plot.title = element_markdown(hjust = 0.5, color = "grey 37", size = 16, face = "bold")) +
    theme(axis.title.y = element_text(color = "grey 37", size = 20)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 16, color = "grey 37"))
  return(g1)
}


#' `plot_list_0_function()`  Creates and prints out 9 area specific graphs by creating a new data frame specific to each area which feeds in to the ggplot
#'
#' @param data_comb_df: data frame with columns for:
#' date, each of 9 areas on interest and their CF values
#' @param reporting_lag_start: currently set as 2021-09-02
#'
#' @param reporting_lag_end: currently set as 2021-10-12
#'
#' @param labels: List of titles for detailing the proportion of tests by affected area
#'
#' @param affected_areas List of all 9 affected areas
#'
#' @param affected_areas_colour colour used the affected areas
#'
#' @paramcounterfactual_areas_colours colour used for the Counterfactual areas

plot_list_0_function <- function(data_comb_df, reporting_lag_start, reporting_lag_end, labels,
                                 affected_areas_colour, counterfactual_areas_colours, y_lab) {
  labels["label"] <- paste0(labels[['Subfigure']],": ",
    labels[["Affected_area"]],
    " (",
    "<span style='color:#009E73;'>", as.character(labels[["prop_tests_in_area"]]),
    "%</span>", ", ",
    as.character(labels[["prop_tests_lab_x"]]), "%)"
  )

  # Adjusting the y axis limits so it's the same for all 9 area specific subplots
  maxylab <- max(subset(data_comb_df, select = names(data_comb_df)[2:length(names(data_comb_df))]))

  plotlist <- list()

  for (i in 1:(nrow(labels) - 1)) {
    data_comb_df_c <- data_comb_df[, c(1, i * 2, (i * 2 + 1))]
    colnames(data_comb_df_c) <- c("date", "Area", "Area_CF")

    p <- ggplot(data = data_comb_df_c, aes(x = date)) +
      geom_line(aes(y = Area), linetype = "solid", color = affected_areas_colour, size = 1.2) +
      geom_line(aes(y = Area_CF), linetype = "dashed", color = counterfactual_areas_colours, size = 1) +
      geom_vline(xintercept = reporting_lag_start, linetype = "dashed") +
      geom_vline(xintercept = reporting_lag_end, linetype = "dashed") +
      theme_minimal(base_family = "Calibri") +
      theme(
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color = "grey 37"),
        plot.title = element_text(hjust = 0.5, color = "grey 37", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        #  text=element_text(family="Calibri")
      ) +
      scale_x_date(date_breaks = "months", date_labels = "%b") +
      ggtitle(paste0(labels[i + 1, 4])) +
      theme(plot.title = element_markdown(hjust = 0.5, face = "bold"))
    # Selecting the plots to add y label
    if (i == 1 | i == 4 | i == 7) {
      p <- p +
        ylab(y_lab) +
        theme(text = element_text(color = "grey 37", size = 14)) +
        ylim(NA, maxylab)
    } else {
      p <- p +
        ylab("") +
        theme(text = element_text(color = "grey 37")) +
        ylim(NA, maxylab)
    }
    plotlist[[i]] <- p
  }

  return(grid.arrange(grobs = plotlist, ncol = 3))
}
