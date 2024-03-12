#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#' @param output_dir 
#' @param cluster 
#' @param resol 
#' @param var.name
#' @param language
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
#' 
missing_data_heatmap <- function(time_series_df, station_info_df, output_dir = "./",xcluster = TRUE, resol = 400, 
                                 var.name = "Precipitation (mm)", language="en", unit="mm") {
  
  # Ensure the 'date' column is in Date format
  time_series_df[[1]] <- as.Date(time_series_df[[1]])
  
  
  
  # Transpose the time_series_df to have station codes as rows
  transposed_df <- time_series_df %>%
    gather(key = "station_code", value = "value", -dates)
  
  # Merge the transposed dataframe with station_info_df on station code
  merged_df <- merge(transposed_df, station_info_df, by.x = "station_code", by.y = "code",all.x=TRUE)
  
  if(xcluster){
    # Order stations by latitude
    station_order <- merged_df %>%
      select(station_code, latitude, cluster) %>%
      distinct() %>%
      arrange(cluster, latitude) %>%
      pull(station_code)
    
    # Estimate missing data for each month on each station
    missing_data <- merged_df %>%
      group_by(station_code, month = format(dates, "%Y-%m"), cluster) %>%
      summarise(missing_count = sum(is.na(value)))
    
  }else{

    # Order stations by latitude
    station_order <- merged_df %>%
      select(station_code, latitude) %>%
      distinct() %>%
      arrange(latitude) %>%
      pull(station_code)
    
    missing_data <- transposed_df %>%
      group_by(station_code, month = format(dates, "%Y-%m")) %>%
      summarise(missing_count = sum(is.na(value)))

  }
  
  
  #titles according to language
  if(language=="en"){
    main_plot <- paste0("Missing Data - ",var.name, " (", unit, ")")
    labelx_plot <- "Month"
    labely_plot <- "Station Code"
    fill_plot <- "Missing Count"
  }else if( language=="es"){
    main_plot <- paste0("Disponibilidad de registros - ",var.name)
    labelx_plot <- "Mes"
    labely_plot <- "Código de la estación"
    fill_plot <- paste0("Conteo de ", "\n","datos vacios")
  }
  
  
  max_value <- max(missing_data$missing_count, na.rm = TRUE)
  missing_data$group <- cut(missing_data$missing_count, breaks = seq(0, 30 + 5, by = 5), include.lowest = T, labels = c("0", "1 - 5", "6 - 10", "11 - 15","16 - 20","21 - 25",">26"))
  
  # Create a single plot with subplots for each cluster (missing data)
  heatmap_plot <- ggplot(missing_data, aes(x = month, y = factor(station_code, levels = station_order), fill = factor(group))) +
    geom_tile() +
    scale_fill_brewer(palette = "YlGnBu") +
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.text = element_text(size = 5), legend.title = element_text(size = 5)) +
    labs(title = main_plot,
         x = labelx_plot,
         y = labely_plot,
         fill = fill_plot) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = .6)) +
    theme(panel.spacing = unit(0.5, "cm"), axis.text.y = element_text(size = 5))+
    theme(axis.text.x = element_text(angle = 90, size = 5)) +
    {if(xcluster)facet_wrap(~cluster, scales = "free_y", ncol = 2)}+# Facet by cluster, allowing y-axis scales to vary
    scale_x_discrete(breaks = unique(missing_data$month)[seq(1, length(unique(missing_data$month)), by = 48)])+
    scale_y_discrete(breaks = station_order[seq(1, length(station_order), by = 2)]) 
  # Save the combined plot as a JPEG file
  ggsave(file.path(paste0(output_dir,var.name, "_avheatmap.jpeg")), heatmap_plot, width = 22, height = 15, units = "cm", dpi = resol)
  
}
