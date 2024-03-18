
#' Title
#'
#' @param time_series_df 
#' @param station_info_df 
#' @param resol 
#' @param var.name 
#' @param output_dir 
#' @param plot 
#' @param unit
#' @param family
#'
#' @return
#' @export
#'
#' @examples
gra_Pettitt <- function(time_series_df,station_info_df, resol = 400, var.name = "",output_dir = "./",
                        xcluster=TRUE, plot=FALSE, language="en", unit="", family=NA) {
  results_list <- list()  # List to store results for each column
  
  for (col in names(time_series_df)[-1]) {  # Exclude the Date column from processing
    est <- col
    
    filtered <- na.omit(time_series_df[,c("date",col)])  # Filter out rows with missing values
    
    pettitt_res <- pettitt.test(filtered[[col]]) # Perform Pettitt test
    
    # Extract relevant information
    result <- pettitt_res
    loc <- filtered$date[result$estimate]
    mu1 = mean(filtered[[col]][1:result$estimate])
    mu2 = mean(filtered[[col]][result$estimate:length(filtered[[col]])])
    p <- result$p.value
    
    # Check significance at 0.05 level
    significant <- ifelse(p< 0.05, "Yes", "No")
    
    # Store the result in a list
    results_list[[col]] <- list(
      'change_point' = loc,
      'p-val' = p,
      'mu1' = mu1,
      'mu2' = mu2,
      'significant' = significant
    )
    
    maver <- ma(time_series_df[[col]],win.len = 12, FUN = mean, na.rm = F)# Calculate moving average
    
    #titles according to language
    if(language=="en"){
      main_plot <- "Pettitt test - Station: %s"
      labelx_plot <- 'Date range: %s to %s'
      legend_plot <- c('Observed',  'm. Average',sprintf('ave 1: %.2f', mu1), sprintf('ave 2: %.2f', mu2), 
                       sprintf('Change point: %s', format(loc, "%Y-%m-%d")))
    }else if( language=="es"){
      main_plot <- "Puebra de Pettitt - Estación: %s"
      labelx_plot <- 'Periodo: %s a %s'
      legend_plot <- c('Observado',  'Media móvil',sprintf('prom 1: %.2f', mu1), sprintf('prom 2: %.2f', mu2), 
                       sprintf('Punto de cambio: %s', format(loc, "%Y-%m-%d")))

    }
    
    if(!plot){
    }else{
      
      # Set up PNG file for each column plot
      if(!is.na(family)){
        png(paste0(output_dir,sprintf("homopet_%s.png", est)),width = 20, height = 10, 
            units = 'cm', res = resol, family=family)  # Change 'pdf' to 'png' if you prefer a different format
      } else {
        png(paste0(output_dir,sprintf("homopet_%s.png", est)),width = 20, height = 10, 
            units = 'cm', res = resol)  # Change 'pdf' to 'png' if you prefer a different format
      }
      

      
      # Visualization (you can customize this part as needed)
      title <- sprintf(main_plot, est)
      label_x <- sprintf( labelx_plot, format(min(filtered$date), "%Y-%m-%d"), format(max(filtered$date), "%Y-%m-%d"))
      label_y <- paste0(var.name, " (", unit, ")")
      
      par(mar = c(2, 2, 1, 1))  # Adjust margins (bottom, left, top, right)
      par(mgp = c(1, 0.5, 0))  # Adjust the distance between the axis label and the axis 
      
      plot(time_series_df$date, time_series_df[[col]], type = 'l', col = '#f0eded', xlab = label_x, ylab = label_y, main = title,
           cex.main = 0.5, cex.lab = 0.5, cex.axis = 0.5 )
      
      lines(time_series_df$date, maver, col = 'blue', lty = 1) # Plotting moving average
      
      # Add lines using segments to control xlim
      segments(min(filtered$date), mu1, loc, mu1, col = 'orange', lty = 2, lw = 1)
      segments(loc, mu2, max(filtered$date), mu2, col = '#336334', lty = 2, lw = 1)
      abline(v = as.numeric(loc), col = 'red', lty = 3, lw = 1.5)
      
      legend("topright", legend = legend_plot,
             col = c('#f0eded','blue', 'orange', '#336334', 'red'),
             lty = c(1,1, 2, 2, 3), cex = 0.5) # Add legend
      
      dev.off()# Close the PNG file
    }
    
  }
  
  # Convert the results list to a data frame
  results_df <- as.data.frame(do.call(rbind, results_list), stringsAsFactors = FALSE)
  
  if(!xcluster){
  }else{
  
    results_df$code<-row.names(results_df)
    time_ser_df <-as.data.frame(merge(results_df, station_info_df, by= "code",all.x=TRUE)) %>%
      select(code,significant,cluster)
    
    # Convert each element in the "significant" list to a factor
    time_ser_df$significant <- sapply(time_ser_df$significant, function(x) as.factor(unlist(x)))
    
    # Create a data frame for ggplot
    ggplot_data <- data.frame(cluster = rep(time_ser_df$cluster, sapply(time_ser_df$significant, length)),
                              significant = unlist(time_ser_df$significant))
    
    # Create the pie chart with labels
    pie_chart <- ggplot(ggplot_data, aes(x = "", fill = significant)) +
      geom_bar(stat = "count", position = "fill", width = 1) +
      coord_polar(theta = "y") +
      facet_wrap(~ cluster) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom"
      ) +
      scale_fill_manual(values = c("lightseagreen", "purple")) +
      geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))), position = position_fill(vjust = 0.5), show.legend = FALSE) +
      labs(title = "Significance Pie Chart by Cluster")
    
    if(!is.na(family)){
      pie_chart <- pie_chart + theme(text=element_text(family=family))
    }
    plot(pie_chart)
    ggsave(file.path(paste0(output_dir,var.name,"piechart.jpeg")),pie_chart, width = 22, height = 15, units = "cm", dpi = resol)
    dev.off()
  }
  
  return(results_df)
  
}

