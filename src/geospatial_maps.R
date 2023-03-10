#' plot_affected_to_counterfactual_map_repel
#'
#' `plot_affected_to_counterfactual_map_repel()` returns a ggplot Choropleth with labels
#' denoting which counterfactual has been used for which affected area, where the labels have repel features
#' dependancies - ggplot2, sf, ggrepel
#'
#' @param rest_of_england: data.frame - UTLA's which are not affected or counterfactual areas (these are coloured grey)
#'
#' @param affected_df: data.frame - UTLA's which are affected (these are coloured red) 
#'
#' @param counterfactual_df: data.frame - UTLA's which are counterfactuals (these are coloured blue)
#' 
#' @param geospatial_map_height: int - height of map
#' 
#' @param geospatial_map_width: int - width of map
#'
#' @param save_map: bool - Save map 
#' 
#' @param path: str - Location for the map to be saved

plot_affected_to_counterfactual_map <- function(rest_of_england_map_df, affected_df,
                                                counterfactual_df,
                                                geospatial_map_height, geospatial_map_width,
                                                save_map) {
  labels <- rbind(affected_df, counterfactual_df)
  plt <- ggplot() +
    ggtitle("B") +
    geom_sf(data = rest_of_england_map_df, fill = alpha("grey", 0.7)) +
    geom_sf(data = affected_df, fill = alpha("red", 0.5)) +
    geom_sf(data = counterfactual_df, fill = alpha("blue", 0.5)) +
    geom_sf() +
    ggrepel::geom_label_repel(
      data = labels,
      aes(label = rank, geometry = centroid), size = 11,
      stat = "sf_coordinates",
      min.segment.length = 0
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 50))

  if (save_map == TRUE) {
    path_to_save <- file.path("outputs","visuals", "geospatial_maps")

    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = "affected_map.png",
      plot = plt,
      path = path_to_save,
      height = geospatial_map_height,
      width = geospatial_map_width
    )
  }

  return(plt)
}

#' plot_bubble_map
#'
#' `plot_bubble_map()` returns a ggplot bubble map where the bubble size denotes the percentage of tests for that UTLA
#' dependancies - ggplot2, sf
#'
#' @param df: data.frame - obtained by runnning read_pct_data
#'
#' @param save_bubble_map: bool - Save map
#'
#' @param bubble_map_height: int - Height of map 
#'
#' @param bubble_map_width: int - Width of map

plot_bubble_map <- function(df, save_bubble_map, bubble_map_height,
                            bubble_map_width) {
  plt <- ggplot() +
    ggtitle("A") +
    geom_sf(data = df, fill = alpha("white")) +
    geom_sf(
      data = df$centroid, pch = 21,
      aes(size = df$percentage_tests_lab),
      fill = alpha("#005EB8", 0.7)
    ) +
    scale_size(
      range = c(1, 20), breaks = c(1, 5,10,20,40), labels = c('<1', '5','10','20','40'),
      guide = guide_legend(
        title = "Percentage of tests\nundertaken by\nthe Laboratory",
        direction = "vertical"
      )
    ) +
    theme_void() +
    theme(
      legend.position = c(.2, .4), legend.title = element_text(size = 35),
      legend.text = element_text(size = 30), plot.title = element_text(hjust = 0.5, size = 50)
    )

  if (save_bubble_map == TRUE) {
    path_to_save <- file.path("outputs","visuals", "geospatial_maps")

    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = "bubble_map.png",
      plot = plt,
      path = path_to_save,
      height = bubble_map_height,
      width = bubble_map_width
    )
  }

  return(plt)
}

#' join_plots
#'
#' `join_plots()` returns a gridExtra plot with Figure A (Bubble Map) and Figure B (Map) side by side
#' dependancies - ggplot2, sf, gridExtra
#'
#' @param bubble_map: ggplot object - obtained by running plot_bubble_map
#'
#' @param cf_to_af_map: ggplot object - obtained by running plot_affected_to_counterfactual_map_repel
#'
#' @param cf_to_af_map: ggplot object - obtained by running plot_affected_to_counterfactual_map_repel
#'
#' @param save_joined_plots: bool - Save map out
#'
#' @param joined_plot_height: int - Height of the map
#'
#' @param joined_plot_width: int - Width of the map
#'
#' @param path: str - location to be saved to

join_plots <- function(bubble_map, cf_to_af_map, save_joined_plots,
                      joined_plot_height, joined_plot_width) {
  joined_plot <- grid.arrange(bubble_map, cf_to_af_map, ncol = 2, 
                              bottom = textGrob("Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data ? Crown copyright and database right 2021",
                                                                                    x = 0.5,
                                                                                    y = 0.5,
                                                                                    just = "centre",gp = gpar(fontsize = 30))) 
  if (save_joined_plots == TRUE) {
    path_to_save <- file.path("outputs","visuals", "geospatial_maps")

    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)

    ggsave(
      filename = "joined_plot.png",
      plot = joined_plot,
      path = path_to_save,
      height = joined_plot_height,
      width = joined_plot_width
    )
  }

  return(joined_plot)
}
