#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#' @param output_dir 
#' @param plot 
#' @param resol 
#' @param var.name
#' @param language
#' @param family
#'
#' @return
#' @export
#'
#' @examples
#' 
station_data_av<- function(time_series_df, station_info_df, output_dir = "./",plot = TRUE, resol = 400,
                           var.name = "Prec (mm)", xcluster = TRUE, language="en",
                           family=NA) {
  
  # Ensure the 'date' column is in Date format
  time_series_df[[1]] <- as.Date(time_series_df[[1]])
  
  # Transpose the time_series_df to have station codes as rows
  transposed_df <- time_series_df %>%
    gather(key = "station_code", value = "value", -dates)
  
  # Merge the transposed dataframe with station_info_df on station code
  merged_df <- merge(transposed_df, station_info_df, by.x = "station_code", by.y = "code", all.x =TRUE)
  
  if(xcluster){
    
    # Order stations by latitude
    station_order <- merged_df %>%
      select(station_code, latitude, cluster) %>%
      distinct() %>%
      arrange(cluster, latitude) %>%
      pull(station_code)
    
    # Step: Calculate the number of available data points each day in all stations
    availability_data <- merged_df %>%
      group_by(dates, cluster) %>%
      summarise(available_count = sum(!is.na(value)))
    
  }else{
    
    availability_data <- merged_df %>%
      group_by(dates) %>%
      summarise(available_count = sum(!is.na(value)))
  }
  
  #titles according to language
  if(language=="en"){
    main_plot <- "Data Availability Over Time"
    labelx_plot <- 'Date'
    labely_plot <- "Number of Available Data Stations"
  }else if( language=="es"){
    main_plot <- "Disponibilidad de registros"
    labelx_plot <- 'Fecha'
    labely_plot <- "Número de estaciones con registro disponible"
  }
  
  if(!plot){
  }else{
    
    # Create a line plot for data availability with subplots for each cluster
    availability_plot <- ggplot(availability_data, aes(x = dates, y = available_count)) +
      geom_line(color = "blue", linewidth = 0.3) +
      labs(title = main_plot,
           x = labelx_plot,
           y = labely_plot) +
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      {if(xcluster)facet_wrap(~cluster, scales = "free_y", ncol = 2)}+# Facet by cluster, allowing y-axis scales to vary
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
      theme(panel.spacing = unit(0.5, "cm"), axis.text.y = element_text(size = 8))+
      scale_x_date(breaks = seq(min(availability_data$dates), max(availability_data$dates), by = "60 month"), date_labels = "%Y")  # Adjust the breaks for x-axis ticks
    if(!is.na(family)){
      availability_plot <- availability_plot + theme(text=element_text(family=family))
    }
  # Save the combined plot as a JPEG file
  ggsave(file.path(paste0(output_dir,var.name,"_sta_av.jpeg")),availability_plot , width = 22, height = 12, units = "cm", dpi = resol)
  }
  
}
