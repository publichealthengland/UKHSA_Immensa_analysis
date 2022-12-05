#' produce_lfd_plot
#'
#' `produce_lfd_plot()` produces daily LFD positivity plot
#'
#' @param df_lfd data.frame - Lateral Flow Testing Data, `initialised_data$df_lfd`
#'  
#' @param df_pcr data.frame - Polymerase Chain Reaction Testing Data `initialised_data$df_pcr`
#' 
#' @param init_data data.frame - Summary Data, `initialised_data$data`
#' 
#' @param reporting_lag_start date - when reporting started
#' 
#' @param reporting_lag_end date - when reporting ended
#' 
#' @param save_line_plots bool - save plot out
#' 
#' @param line_plot_height int - height of saved plot
#' 
#' @param line_plot_width int - width of saved plot
#' 
produce_lfd_plot <- function(df_lfd, df_pcr, init_data,
                                               reporting_lag_start, reporting_lag_end,save_line_plots,
                                               line_plot_height, line_plot_width) {
  plot_data <- get_data_ready_for_plots(df_lfd, df_pcr, init_data, reporting_lag_start, reporting_lag_end)

  daily_LFD_positivity <- plot_daily_LFD_positivity(
    df = left_join(x = plot_data$df_lfd_plot, y = plot_data$df_prop_lab_x),
    plot_regions = c("South West", "West Midlands", "South East"),
    plot_y_limits = c(0, 5),
    lab_x_start = reporting_lag_start,
    lab_x_end = reporting_lag_end,
    lab_x_ribbon_alpha = 0.3
  )

  if (save_line_plots == TRUE) {
    path_to_save <- file.path("outputs","visuals", "line_plots")

    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = "LFD_positivity.png",
      plot = daily_LFD_positivity,
      path = path_to_save,
      height = line_plot_height,
      width = line_plot_width
    )
  
  }

  return(list(daily_LFD_positivity = daily_LFD_positivity))#, daily_hosp_admissions = daily_hosp_admissions, daily_deaths = daily_deaths))
}

#' theme_custom_lab_x
#'
#' `theme_custom_lab_x()` returns a custom theme which strips the background and places a legend in the top left.
#' used to ensure consistency of plots
#'
theme_custom_lab_x <- function(base_size = 16) {
  theme_classic(base_size, base_family = "Calibri") +
    theme(
      strip.background = element_blank(),
      legend.position = c(0.2, 0.8)
    )
}


#' plot_daily_LFD_positivity
#'
#' `plot_daily_LFD_positivity()` returns a ggplot with date on the y axis and the LFD positivity % on the y axis.
#' dependancies - dplyr, ggplot2, scales
#'
#' @param df subsetted regional data-frame to be plotted, with the following variables;
#' date, lab_x_cat, ltla_code, lab_x_positivity_25, lab_x_positivity_50, lab_x_positivity_75
#'
#' @param plot_regions vector of regions to be included in the plot, e.g. may want to restrict analysis to South East.
#'
#' @param lab_x_ribbon_alpha sets alpha for the ribbon from 25th to 75th percentile
#'
#' @param theme_custom is included as a custom theme.
#'
#' @param lab_x_start & lab_x_end set the lines for start and end of lab_x period in the plot
#'
#' @param colour_scheme sets the colours for the different categories, names must match categories set in steps above.
#'
#' @param plot_y_limits sets the limits on the y axis.
plot_daily_LFD_positivity <- function(df = df_lfd_plot,
                                      plot_regions = c("South West", "West Midlands", "South East"),
                                      lab_x_ribbon_alpha = 0.2,
                                      theme_custom = theme_custom_lab_x,
                                      lab_x_start = as.Date("2021-09-02"),
                                      lab_x_end = as.Date("2021-10-16"),
                                      colour_scheme = c("(0,5]" = "grey60", "(5,20]" = "#00BFC4", "(20,100]" = "#F8766D"),
                                      plot_y_limits = c(0, 5)) {

  # filter the regions to include only the areas of interest, keep only these regions for the points
  df_plot_points <- df %>% filter(region %in% plot_regions)

  # get the category quantiles (25% LTLA, 50% LTLA, 75% LTLA etc) for the lines
  df_plot_lines <- get_LTLA_quantiles_Positivity(df_plot_points)

  # get the labels for the legend
  prop_labels <- get_lab_xprop_labels(df_plot_points)


  # create the plot, uses ggplot2
  ggplot() +
    geom_line(
      data = df_plot_lines,
      aes(
        x = date,
        y = lab_x_positivity_50*100,
        col = lab_x_cat
      ),
      size = 2
    ) +
    geom_ribbon(
      data = df_plot_lines,
      aes(
        x = date,
        ymin = lab_x_positivity_25*100,
        ymax = lab_x_positivity_75 *100,
        fill = lab_x_cat
      ),
      alpha = lab_x_ribbon_alpha
    ) +
    geom_point(
      data = df_plot_points,
      aes(
        x = date,
        y = positivity*100,
        col = lab_x_cat
      ),
      alpha = 0.25, pch = 20
    ) +
    theme_custom() + 
    annotate("segment",
               x = lab_x_start, xend = lab_x_start, y = 0, yend = Inf, linetype="dashed",
    ) +
    annotate("segment",
             x = lab_x_end, xend = lab_x_end, y = 0, yend = Inf, linetype="dashed",
    ) +
    annotate(
      geom = "text",
      x = mean(c(lab_x_start, lab_x_end)),
      y = 4.5,
      label = "Period the\nLaboratory\nwas operational", size = 6
    ) +
    scale_y_continuous(name = "Proportion of daily LFD tests that are positive (per cent)", #labels = scales::percent, 
                       limits = plot_y_limits
                       ) +
    scale_x_date(name = "", limits = c(min(df_plot_lines$date) + 3, max(df_plot_lines$date) - 3)) +
    scale_colour_manual(
      aesthetics = c("fill", "colour"),
      labels = prop_labels,
      values = colour_scheme
    ) +
    labs(
      col = "per cent of tests at \n Laboratory X",
      caption = paste("Shaded area shows interquartile range for LTLAs in the group",paste("\nRegions included:", paste(plot_regions, collapse = " & ")))
    ) +
    guides(alpha = "none", fill = "none") +
    theme(legend.title = element_text(size = 22),legend.text = element_text(size = 17),
      plot.subtitle = element_text(size = 14)
    )
}
