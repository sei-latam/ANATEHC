
#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#'
#' @return
#' @export
#'
#' @examples
missing_data_heatmap <- function(time_series_df, station_info_df) {
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
  
  # Step 2: Estimate missing data for each month on each station
  missing_data <- merged_df %>%
    group_by(station_code, month = format(date, "%Y-%m"), cluster) %>%
    summarise(missing_count = sum(is.na(value)))
  
  heatmaps <- list()
  for (clust in unique(missing_data$cluster)) {
    subset_data <- filter(missing_data, cluster == clust)
    
    heatmap_plot <- ggplot(subset_data, aes(x = month, y = factor(station_code, levels = station_order), fill = factor(missing_count))) +
      geom_tile() +
      scale_fill_brewer(palette = "YlOrRd", na.value = "grey50") +
      labs(title = paste("Missing Data Heatmap - Cluster", clust),
           x = "Month",
           y = "Station Code",
           fill = "Missing Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    heatmaps[[as.character(clust)]] <- heatmap_plot
  }
  
  return(heatmaps)
}

