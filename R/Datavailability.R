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
station_data_av<- function(time_series_df, station_info_df, output_dir = "./") {
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
  
  # Step: Calculate the number of available data points each day in all stations
  availability_data <- merged_df %>%
    group_by(date, cluster) %>%
    summarise(available_count = sum(!is.na(value)))
  
  # Create a line plot for data availability with subplots for each cluster
  availability_plot <- ggplot(availability_data, aes(x = date, y = available_count)) +
    geom_line(color = "blue", linewidth = 0.3) +
    labs(title = "Data Availability Over Time",
         x = "Date",
         y = "Number of Available Data Stations") +
    facet_wrap(~cluster, scales = "free_y", ncol = 2) +  # Subplots for each cluster
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))+
    theme(panel.spacing = unit(0.5, "cm"), axis.text.y = element_text(size = 8))+
    scale_x_date(breaks = seq(min(availability_data$date), max(availability_data$date), by = "12 month"), date_labels = "%Y")  # Adjust the breaks for x-axis ticks

  
  # Save the combined plot as a JPEG file
  ggsave(file.path(output_dir, "stations_Availability.jpeg"),availability_plot , width = 22, height = 15, units = "cm", dpi = 400)
  
}
