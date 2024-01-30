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
#' 
stats_per_cluster <- function(time_series_df, station_info_df, output_dir = "./") {
  # Extract relevant columns from time_series_df
  time_series_data <- time_series_df[, -1]
  
  # Calculate fivenumber statistics for all stations
  fivenum_values <- apply(time_series_data, 2, fivenum, na.rm = TRUE)
  
  # Extract the "cluster" column from station_info_df
  cluster_column <- station_info_df$cluster
  
  # Combine fivenum_values with the "cluster" column
  result_df <- data.frame(
    code = station_info_df$code,
    name = station_info_df$name,
    cluster = cluster_column,
    mean_value = colMeans(time_series_data, na.rm = TRUE),  # Mean
    min_value = fivenum_values[1,],         # Min
    #lower_hinge = fivenum_values[2,],       # Lower hinge
    median_value = fivenum_values[3,],      # Median
    #upper_hinge = fivenum_values[4,],       # Upper hinge
    max_value = fivenum_values[5,],         # Max
    sd_value = apply(time_series_data, 2, sd, na.rm = TRUE)  # Standard deviation
  )
  
  # Create a long-format data frame suitable for plotting
  result_melted_df <- reshape2::melt(result_df, id.vars = c("code", "name", "cluster"))
  
  # Create the plot
  p <- ggplot(result_melted_df, aes(x = factor(cluster), y = value, fill = factor(cluster))) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_boxplot(outlier.colour = "red", fill = '#A4A4A4', color = "black", ) +
    facet_wrap(~variable, scales = "free_y", ncol = 3, strip.position = "left") +
    labs(title = "Statistics", x = "Cluster") +
    ylab("Value") +
    theme(panel.spacing = unit(0.5, "cm"), 
          axis.text.y = element_text(size = 8))
  
  # Save the plot as a JPEG file
  jpeg(file.path(output_dir, "boxplot_statistics.jpeg"), width = 18, height = 15, units = "cm", res = 400)
  print(p)
  dev.off()
  
  return(p)
}

