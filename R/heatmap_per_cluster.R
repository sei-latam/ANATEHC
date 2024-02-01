#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
missing_data_heatmap <- function(time_series_df, station_info_df, output_dir = "./") {
  # Ensure the 'date' column is in Date format
  time_series_df$date <- as.Date(time_series_df$date)
  
  # Transpose the time_series_df to have station codes as rows
  transposed_df <- time_series_df %>%
    gather(key = "station_code", value = "value", -date)
  
  # Merge the transposed dataframe with station_info_df on station code
  merged_df <- merge(transposed_df, station_info_df, by.x = "station_code", by.y = "code")
  
  # Order stations by latitude
  station_order <- merged_df %>%
    select(station_code, latitude, cluster) %>%
    distinct() %>%
    arrange(cluster, latitude) %>%
    pull(station_code)
  
  # Estimate missing data for each month on each station
  missing_data <- merged_df %>%
    group_by(station_code, month = format(date, "%Y-%m"), cluster) %>%
    summarise(missing_count = sum(is.na(value)))
  
  # Create a single plot with subplots for each cluster (missing data)
  heatmap_plot <- ggplot(missing_data, aes(x = month, y = factor(station_code, levels = station_order), fill = factor(missing_count))) +
    geom_tile() +
    scale_fill_brewer(palette = "YlGnBu",)+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
    labs(title = "Missing Data Heatmap",
         x = "Month",
         y = "Station Code",
         fill = "Missing Count") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(panel.spacing = unit(0.5, "cm"), axis.text.y = element_text(size = 8))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~cluster, scales = "free_y", ncol = 2)+# Facet by cluster, allowing y-axis scales to vary
    scale_x_discrete(breaks = unique(missing_data$month)[seq(1, length(unique(missing_data$month)), by = 12)]) 
  
  # Save the combined plot as a JPEG file
  ggsave(file.path(output_dir, "combined_heatmap.jpeg"), heatmap_plot, width = 22, height = 15, units = "cm", dpi = 400)

}
