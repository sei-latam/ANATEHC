
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
#' 
gra_HOMO_Pettitt_multi_columns <- function(df) {
  results_list <- list()  # List to store results for each column
  
  for (col in names(df)[-1]) {  # Exclude the Date column from processing
    est <- col
    
    
    filtered <- na.omit(df[,c("date",col)])  # Filter out rows with missing values
    
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
    
    maver <- ma(df[[col]],win.len = 12, FUN = mean, na.rm = F)# Calculate moving average
    
    # Set up PNG file for each column plot
    png(sprintf("11plot_%s.png", est),width = 20, height = 10, units = 'cm', res = 400)  # Change 'pdf' to 'png' if you prefer a different format
    
    # Visualization (you can customize this part as needed)
    title <- sprintf("Pettitt test - Station: %s", est)
    label_x <- sprintf('Date range: %s to %s', format(min(filtered$date), "%Y-%m-%d"), format(max(filtered$date), "%Y-%m-%d"))
    label_y <- "Precipitation [mm]"
    
    par(mar = c(2, 2, 1, 1))  # Adjust margins (bottom, left, top, right)
    par(mgp = c(1, 0.5, 0))  # Adjust the distance between the axis label and the axis 
    
    plot(df$date, df[[col]], type = 'l', col = '#f0eded', xlab = label_x, ylab = label_y, main = title,
         cex.main = 0.5, cex.lab = 0.5, cex.axis = 0.5 )
    
    # Add lines using segments to control xlim
    segments(min(filtered$date), mu1, loc, mu1, col = 'orange', lty = 2, lw = 1)
    segments(loc, mu2, max(filtered$date), mu2, col = '#336334', lty = 2, lw = 1)
    abline(v = as.numeric(loc), col = 'red', lty = 3, lw = 2)
    
    lines(df$date, maver, col = 'blue', lty = 1) # Plotting moving average

    legend("topright", legend = c('Observed',  'm. Average',sprintf('ave 1: %.2fmm', mu1), sprintf('ave 2: %.2fmm', mu2), 
                                  sprintf('Change point: %s', format(loc, "%Y-%m-%d"))),
           col = c('#f0eded','blue', 'orange', '#336334', 'red'),
           lty = c(1,1, 2, 2, 3), cex = 0.5) # Add legend
    
    dev.off()# Close the PNG file
    
  }
  
  # Convert the results list to a data frame
  results_df <- as.data.frame(do.call(rbind, results_list), stringsAsFactors = FALSE)
  
  return(results_df)
}

