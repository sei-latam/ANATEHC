
#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
data_availability_plot <- function(time_series_df, station_info_df) {
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
  
  # Step 2: Calculate the number of available data points each day in all stations
  availability_data <- merged_df %>%
    group_by(date) %>%
    summarise(available_count = sum(!is.na(value)))
  
  # Create a line plot for data availability
  availability_plot <- ggplot(availability_data, aes(x = date, y = available_count)) +
    geom_line() +
    labs(title = "Data Availability Over Time",
         x = "Date",
         y = "Number of Available Data Points") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(availability_plot)
}
