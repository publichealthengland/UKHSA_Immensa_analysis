plot_ltla_region_swash <- function(df, regions, test_type, save, path_to_save) {
  df <- df %>% filter((region %in% regions) & (daily_growth_rate))

  plt <- ggplot(data = df, aes(x = percentage_tests_lab, y = daily_growth_rate)) + # ,color=region
    geom_point(aes(colour = region), size = 3) +
    # geom_smooth()+
    stat_smooth(
      method = "lm", se = TRUE,
      formula = y ~ poly(x, 3, raw = TRUE), colour = "black"
    ) +
    labs(color = "Region") +
    xlab("\nPercentage of Tests Processed by the Laboratory") +
    ylab(paste0(test_type, " Positivity Daily Growth Rate (per cent)\n")) + theme_bw() + 
    theme(legend.position = c(0.91, .1),
          legend.title = element_text(size=18),
          legend.text = element_text(size=15),
          plot.title = element_text(size=20),
          # axis.title.x = element_text(size=10),
          axis.text = element_text(size=13),
          axis.title = element_text(size = 15),
          # axis.title.y = element_text(size=15),# face = 'bold'
    legend.background = element_rect(fill="white",
    linetype="solid", 
    colour ="black")) + guides(colour = guide_legend(override.aes = list(size = 3)))
  
  if (save == TRUE) {
    path_to_save <- file.path("outputs","visuals", "daily_growth")
    
    dir.create(file.path("outputs","visuals"), showWarnings = FALSE)
    dir.create(path_to_save, showWarnings = FALSE)
    
    ggsave(
      filename = "ltla_daily_growth_rate.png",
      plot = plt,
      path = path_to_save,
      height = 8,
      width = 12
    )
    }
  return(plt)
}
