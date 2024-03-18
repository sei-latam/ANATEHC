
#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#' @param output_dir 
#' @param plot 
#' @param resol 
#' @param var.name 
#' @param remove.zero 
#'
#' @return
#' @export
#'
#' @examples
#' 
stats_per_cluster <- function(time_series_df, station_info_df, output_dir = "./",plot = TRUE, resol = 400, var.name = "",
                              xcluster = TRUE, remove.zero = TRUE) {


  # Optionally remove zero values from the data
  if(remove.zero){
    data.1 <- as.data.frame(apply(time_series_df[,-1], 2, function(x) replace(x, x %in% 0, NA)))
    data.1$dates <- time_series_df[,1]
    time_series_df <- data.1[,c("dates",colnames(data.1[,-c(ncol(data.1))]))]
  } else {
  }
  
  # Extract relevant columns from time_series_df
  time_series_data<- time_series_df[, -1]
  # Calculate fivenumber statistics for all stations
  fivenum_values <- apply(time_series_data, 2, fivenum, na.rm = TRUE)
  
  # Combine fivenum_values with the "cluster" column
  result_df <- data.frame(
    code = colnames(fivenum_values),
    mean = colMeans(time_series_data, na.rm = TRUE),  # Mean
    min = fivenum_values[1,],         # Min
    median = fivenum_values[3,],      # Median
    max = fivenum_values[5,],         # Max
    sd = apply(time_series_data, 2, sd, na.rm = TRUE)  # Standard deviation
  )
  
  result_df<-merge(result_df,station_info_df,by = "code",all.x=TRUE)
  result_df<-result_df[,c("code", "name", "cluster","mean","min","max","sd","median")]
  
  if(!plot){
  }else{
    
    # Create a long-format data frame suitable for plotting
    result_melted_df <- reshape2::melt(result_df, id.vars = c("code", "name", "cluster"))
    
    # Create the plot
    p <- ggplot(result_melted_df, aes(x = factor(cluster), y = value, color = factor(cluster))) +
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=.5)) +
      geom_boxplot(outlier.colour = "black", fill = '#e0dede', outlier.size = 0.5, linewidth= 0.5) +
      scale_colour_brewer(palette = "Dark2",)+
      {if(xcluster)facet_wrap(~cluster, scales = "free_y", ncol = 2)}+# Facet by cluster, allowing y-axis scales to vary
      labs(title = "Statistics",x=NULL) +
      ylab(paste0("Value_",var.name)) +
      theme(panel.spacing = unit(0.5, "cm"), 
            axis.text.y = element_text(size = 8))+
      theme(legend.position = "bottom",legend.key.size = unit(.3, 'cm'))+
      guides(color=guide_legend(title="Cluster"))
    
    # Save the plot as a JPEG file
    ggsave(file.path(output_dir,paste0(var.name,"_bxp_stats.jpeg")),p , width = 12, height = 15, units = "cm", dpi = resol)
    
  }
  
}

