#
#' merge_ci_model_grid_fig
#'
#' `merge_ci_model_grid_fig()`  Combines the above 9 plots produced in for loop into a single grid
#'
#' @param use_common_legend  select if you want a to use a common legend
#'
#' @param data_new data.frame - Data returned from Causal Impact model, stored in `ci_data_sets$data_new` 
#' 
#' @param ci_new data.frame - Data returned from Causal Impact model, stored in `ci_data_sets$ci_new`
#'  
#' @param prop_tests_in_area List of the proportion of tests in the affected areas
#' 
#' @param prop_tests_lab_x List of proportion of tests
#' 
#' @param reporting_lag_start date - date the reporting lag started
#' 
#' @param reporting_lag_end date - date the reporting lag ended
#' 
#' @param ci_start date - Date to start Causal Impact Analysis
#' 
#' @param selected_areas List - Affected Area and their respective counterfactual areas
#' 
#' @param rest_of_england bool - use the rest of england for Causal Impact Analysis 
#' 
#' @param surrounding_areas List of affected areas and their counterfactuals
#' 
#' @param knn_neighbours int - number of counterfactuals to use 
#' 
#' @param individual_series bool - Utilise individual series or not
#' 
#' @param data data.frame - Summary Data, `initialised_data$data`
#' 
#' @param apply_log bool - Apply log transformation 
#' 
#' @param log_const int - the log constant
#' 
#' @param affected_areas_colour str - Colour to plot the affected areas
#' 
#' @param synthetic_counterfactual_colour str - Colour to plot the synthetic counterfactual areas
#' 
#' @param impact_measure str - Impact measure for plot title 
#' 
#' @param labels list - A list of titles for detailing the proportion of tests by affected area
#' 
#' @param grid_plot_height int - height of saved plots
#' 
#' @param grid_plot_width int - width of saved plots 
#' 
#' @param save_ci_graphs: bool - Save out Causal impact graphs

merge_ci_model_grid_fig <- function(data_new, ci_new, prop_tests_in_area, prop_tests_lab_x,
                                    reporting_lag_start, reporting_lag_end, ci_start,
                                    selected_areas, rest_of_england, surrounding_areas,
                                    knn_neighbours, individual_series, data,
                                    apply_log, log_const, use_common_legend,
                                    affected_areas_colour, synthetic_counterfactual_colour,
                                    impact_measure, labels, grid_plot_height, grid_plot_width, save_ci_graphs,
                                    data_comb_df, revert_log) {
  
  if (impact_measure == "lfd_positivity") {
    y_lab <- "LFD Positivity (per cent)"
  } else if (impact_measure == "admissions_per_100k") {
    y_lab <- "Admissions (per 100k)"
  } else if (impact_measure == "deaths_per_100k") {
    y_lab <- "Deaths (per 100k)"
  }
  
  if ((apply_log == TRUE) & (revert_log==FALSE)) {
    y_lab <- paste0("log(", log_const, " + ", y_lab, ")\n")
  }

  g1 <- causal_modelling_plot_function(
    data_new, ci_new, prop_tests_in_area, prop_tests_lab_x,
    reporting_lag_start, reporting_lag_end, ci_start, y_lab, revert_log, log_const
  )

  # need to go into this and check for when individual_series = FALSE to ensure it works
  g2 <- causal_model_plot_list(
    labels, selected_areas, rest_of_england, surrounding_areas,
    knn_neighbours, individual_series, data,
    apply_log, log_const, affected_areas_colour, synthetic_counterfactual_colour, y_lab,
    impact_measure, reporting_lag_start, reporting_lag_end, data_comb_df, revert_log
  )


  # Combining all graphs together and adding a single legend for all
  final_plot_data <- ggarrange(g1, # First row with the Figure 1
    ggarrange(g2, ncol = 1), # Second row with the area plots
    nrow = 2, common.legend = use_common_legend, legend = "bottom"
  ) + theme(plot.margin = margin(0.1, 0.1, 2, 0.1, "cm"))

  if (save_ci_graphs == TRUE) {
    path_to_save <- file.path("outputs","visuals", "ci_plots")

    dir.create(file.path( "outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = paste0(impact_measure, ".png"),
      plot = final_plot_data,
      path = path_to_save,
      height = grid_plot_height,
      width = grid_plot_width
    )
  }

  return(final_plot_data)
}

#' causal_modelling_plot_function
#'
#' `causal_modelling_plot_function()`  Plot all Affected areas and Counterfactuals
#'
#' @param ci_start  staring date for confidence interval on the plot
#'

causal_modelling_plot_function <- function(data_new, ci_new, prop_tests_in_area, prop_tests_lab_x,
                                           reporting_lag_start, reporting_lag_end, ci_start, y_lab, revert_log, log_const) {
  # Setting the 'Date' COlumn to the date format
  ci_new$date <- as.Date(ci_new$date)
  row.names(ci_new) <- NULL
  ci_new_2 <- ci_new[row.names(ci_new[ci_new$date == ci_start, ]):length(ci_new$date), ]
  
  # # exp transformation
  if (revert_log == TRUE) {
    # convert both back to normal scale
    data_new <- as.data.frame(lapply(data_new, exp_fun, log_const))

    ci_new_2 <- as.data.frame(lapply(ci_new_2, exp_fun, log_const))

    ci_new <- as.data.frame(lapply(ci_new, exp_fun, log_const))
  }
  
  g <- ggplot(data = ci_new, aes(y = `point.pred`, x = `date`)) +
    geom_line(aes(color = "Synthetic Comparator"), linetype = "dashed", size = 1) +
    geom_line(aes(y = data_new[["All.affected.areas"]], color = "Affected areas"),
      linetype = "solid", size = 1.2
    ) +
    geom_line(
      data = ci_new_2, aes(y = `point.pred.lower`, color = "Lower"),
      linetype = "solid", size = 0.1
    ) +
    geom_line(
      data = ci_new_2, aes(y = `point.pred.upper`, color = "Upper"),
      linetype = "solid", size = 0.1
    ) +
    geom_ribbon(
      data = ci_new_2, aes(ymin = `point.pred.lower`, ymax = `point.pred.upper`),
      fill = "grey 37", alpha = .15
    ) +
    scale_color_manual(
      name = "",
      values = c("Affected areas" = "#d62728", "Synthetic Comparator" = "green")
    ) +
    geom_vline(xintercept = reporting_lag_start, linetype = "dashed") +
    geom_vline(xintercept = reporting_lag_end, linetype = "dashed") +
    theme_minimal()+#base_family = "Calibri") +
    theme(axis.title.x = element_blank()) + # ,plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    theme(axis.line.x.bottom = element_line(color = "grey 37")) +
    ylab(paste0(y_lab, "\n")) +
    labs(title = paste0(
      "A: Top nine affected UTLAs combined <br/> (",
      "<span style='color:#009E73;'>", prop_tests_in_area[[1]], "%</span>",
      " of tests in UTLAs associated with the Laboratory,<br/> accounting for ",
      prop_tests_lab_x[[1]], " per cent of all Laboratory X tests)"
    )) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        color = "grey 37", size = 16
      )
    ) +
    theme(plot.title = element_markdown(hjust = 0.5, face = "bold")) +
    theme(axis.title.y = element_text(color = "grey 37", size = 20)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(legend.text = element_text(size = 16, color = "grey 37"))
  return(g)
}

#' causal_model_plot_list
#'
#' `causal_model_plot_list()`  This is for each individual affected areas vs its counterfactuals
#'
#' @param affected_areas_colour str - colour used the affected areas
#' 
#' @param synthetic_counterfactual_colour str - colour used for Synthetic Comparator
#' 
#' @param labels list - A list of titles for detailing the proportion of tests by affected area
#' 
#' @param selected_areas List - Affected Area and their respective counterfactual areas
#' 
#' @param rest_of_england List of affected areas and their counterfactuals
#' 
#' @param surrounding_areas List of affected areas and their counterfactuals
#' 
#' @param knn_neighbours int - Number of KNN neighours to use for Causal Impact analysis
#' 
#' @param individual_series bool - Utilise individual series or not
#' 
#' @param data data.frame - Summary Data, `initialised_data$data`
#' 
#' @param apply_log bool - Apply log transformation 
#' 
#' @param log_const int - the log constant
#' 
#' @param y_lab str - Y label 
#' 
#' @param impact_measure str - Impact measure for plot title 
#' 
#' @param reporting_lag_start date - date the reporting lag started
#' 
#' @param reporting_lag_end date - date the reporting lag ended
#' 
causal_model_plot_list <- function(labels, selected_areas, rest_of_england, surrounding_areas,
                                   knn_neighbours, individual_series, data,
                                   apply_log, log_const, affected_areas_colour,
                                   synthetic_counterfactual_colour, y_lab, impact_measure,
                                   reporting_lag_start, reporting_lag_end, data_comb_df, revert_log) {
  plot_list <- list()
  lab_list <- list()

  affected_areas <- labels[["Affected_area"]][2:nrow(labels)]

  for (i in 1:(nrow(labels) - 1))
  {
    if (rest_of_england == FALSE) {
      counterfactual_areas_loop <-
        c(affected_areas[[i]], as.list(strsplit(selected_areas[["Counterfactual.areas"]][[i]], ", "))[[1]])
      
      if (surrounding_areas == FALSE) {
        counterfactual_areas_loop <- counterfactual_areas_loop[1:(knn_neighbours)]
      } 
    } else {
      counterfactual_areas_loop <- c(affected_areas[[i]], unique(data[data$utla %ni% selected_areas$Affected.areas, ][["utla"]]))
    }
   
    if (individual_series == TRUE) {
      data_loop <- data[data$utla %in% counterfactual_areas_loop, ] %>%
        select(utla, date, impact_measure) %>%
        pivot_wider(
          names_from = "utla",
          values_from = impact_measure
        ) %>%
        .[, c("date", counterfactual_areas_loop)]
  
      if (apply_log == TRUE) {
        data_loop$plot_col <- adjusted_log(data_loop[[affected_areas[[i]]]], log_const)
      } else {
        data_loop$plot_col <- data_loop[[affected_areas[[i]]]]
      }
    } else {
      # what should this be instead of data_comb_df?
      data_loop <- data_comb_df %>%
        select(
          date, affected_areas[[i]],
          paste0(affected_areas[[i]], "_CF")
        )
      if (apply_log == TRUE) {
        data_loop$plot_col <- adjusted_log(data_loop[[affected_areas[[i]]]], log_const)
      } else {
        data_loop$plot_col <- data_loop[[affected_areas[[i]]]]
      }
    }

    data_loop$date <- as.Date(data_loop$date, format = "%d/%m/%Y")

    # Defines the min and max for the vline based on the plotted variable
    min_vline <- min(data_loop$plot_col[!is.na(data_loop$plot_col)])
    max_vline <- max(data_loop$plot_col[!is.na(data_loop$plot_col)])
    # Gets the data ready for Causal Impact modelling
    ci_data_loop <- causalimpactprep(
      subset(data_loop, select = -c(plot_col)),
      reporting_lag_start,
      apply_log,
      log_const
    )
    
    # Conducts the Causal Impact model
    ci_loop <- CausalImpact(
      ci_data_loop$time_df,
      ci_data_loop$preperiod,
      ci_data_loop$postperiod,
      model.args = list(niter = 10000)
    )
    # Gets the Causal Impact output ready for plotting
    ci_loop <- as.data.frame(ci_loop$series) %>%
      cbind(date = rownames(.), .)

    # Each individual plot corresponds to a particular affected area - ggplots
    ci_loop$date <- as.Date(ci_loop$date)
    row.names(ci_loop) <- seq(1:length(ci_loop$date))
    ci_loop_2 <- ci_loop[row.names(ci_loop[ci_loop$date == "2021-09-02", ]):length(ci_loop$date), ]
    
    if ((apply_log == TRUE) & (revert_log == TRUE)) {
      # convert both back to normal scale
      data_loop <- as.data.frame(lapply(data_loop, exp_fun, log_const))
      
      ci_loop_2 <- as.data.frame(lapply(ci_loop_2, exp_fun, log_const))
    }
    
    labels["label"] <- paste0(labels[["Subfigure"]],': ',
      labels[["Affected_area"]],
      " (",
      "<span style='color:#009E73;'>", as.character(labels[["prop_tests_in_area"]]),
      "%</span>", ", ",
      as.character(labels[["prop_tests_lab_x"]]), "%)"
    )

    g_final <- ggplot() +
      geom_line(
        data = data_loop, aes(y = `plot_col`, x = `date`, color = "Affected areas"),
        show.legend = FALSE, linetype = "solid", size = 1.2
      ) +
      geom_line(
        data = ci_loop_2, aes(y = `point.pred`, color = "Synthetic Comparator", x = `date`),
        show.legend = FALSE, linetype = "dashed", size = 1
      ) +
      geom_line(data = ci_loop_2, aes(y = `point.pred.upper`, x = `date`), size = .1) +
      geom_line(data = ci_loop_2, aes(y = `point.pred.lower`, x = `date`), size = .1) +
      geom_ribbon(
        data = ci_loop_2, aes(ymin = `point.pred.lower`, ymax = `point.pred.upper`, x = `date`),
        fill = "grey 37", alpha = .15
      ) +
      scale_color_manual(
        name = "",
        values = c(
          "Affected areas" = affected_areas_colour,
          "Synthetic Comparator" = synthetic_counterfactual_colour
        )
      ) +
      geom_vline(xintercept = reporting_lag_start, linetype = "dashed") +
      geom_vline(xintercept = reporting_lag_end, linetype = "dashed") +
      theme_minimal()+#base_family = "Calibri") +
      theme(
        axis.title.x = element_blank(),
        axis.line.x.bottom = element_line(color = "grey 37"),
        plot.title = element_text(hjust = 0.5, color = "grey 37", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), text = element_text(family = "Calibri")
      ) +
      scale_x_date(date_breaks = "months", date_labels = "%b") +
      ggtitle(paste0(labels[i + 1, 4])) +
      theme(plot.title = element_markdown(hjust = 0.5, face = "bold"))
    # Selecting the plots  to add y label
    if (i == 1 | i == 4 | i == 7) {
      g_final <- g_final +
        ylab(y_lab) +
        theme(text = element_text(color = "grey 37"))
    } else {
      g_final <- g_final +
        ylab("") +
        theme(text = element_text(color = "grey 37"))
    }

    plot_list[[i]] <- g_final
  }

  return(grid.arrange(grobs = plot_list, ncol = 3))
}
