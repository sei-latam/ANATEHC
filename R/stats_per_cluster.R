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
stats_per_cluster <- function(time_series_df, station_info_df) {
  # Extract the station codes from the time series dataframe
  station_codes <- colnames(time_series_df)[-1]  # Exclude the 'date' column
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame(code = character(), 
                          mean_value = numeric(), min_value = numeric(), max_value = numeric(),
                          stringsAsFactors = FALSE)
  
  # Iterate through each station code
  for (code in station_codes) {
    # Extract time series data for the current station
    station_data <- time_series_df[c("date", code)]
    
    # Calculate mean, min, and max values for the current station
    mean_value <- mean(station_data[[code]], na.rm = TRUE)
    min_value <- min(station_data[[code]], na.rm = TRUE)
    max_value <- max(station_data[[code]], na.rm = TRUE)
    
    # Extract station information
    station_info <- subset(station_info_df, code == colnames(station_data)[2])
    
    # Create a row with the results and station information
    result_row <- data.frame(
      code = station_info$code,
      name = station_info$name,
      cluster = station_info$cluster,
      mean_value = mean_value,
      min_value = min_value,
      max_value = max_value,
      stringsAsFactors = FALSE
    )
    
    # Append the row to the result dataframe
    result_df <- rbind(result_df, result_row)
  }
  
  
  result_melted_df <- reshape2::melt(result_df, id.vars = c("code", "name", "cluster"))
 
  # Create a boxplot for each variable, faceted by cluster
  plots <- lapply(unique(result_melted_df$cluster), function(cluster_value) {
    ggplot(subset(result_melted_df, cluster == cluster_value), 
           aes(x = variable, y = value)) +
      geom_boxplot() +
      labs(title = paste("Boxplots for Media, Min, and Max Values - Cluster", cluster_value),
           x = "Variable", y = "Value")
  })
  
  return(plots)
}

