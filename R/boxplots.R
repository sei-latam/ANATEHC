
#' Title
#'
#' @param data 
#' @param output_dir 
#' @param plot 
#' @param resol 
#' @param var.name 
#' @param remove.zero 
#' @param family
#'
#' @return
#' @export
#'
#' @examples
flow_boxp <- function(data, output_dir = "./",plot = TRUE, resol = 400, var.name = "Prec (mm)",
                      remove.zero = TRUE, family=NA) {
  
  
  
  # Optionally remove zero values from the data
  if(remove.zero){
    data.1 <- as.data.frame(apply(data[,-1], 2, function(x) replace(x, x %in% 0, NA)))
    data.1$dates <- data[,1]
    data <- data.1[,c("dates",colnames(data.1[,-c(ncol(data.1))]))]
  } else {
  }
  
  # Extract relevant columns from data
  time_series_data<- data[, -1]
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
  
  box.stats <- apply(data[,-1], 2,function(x, y) boxplot(x~y, outline=F,plot = FALSE,range=1.5), y = format(data[,1],'%m'))

  if(!plot){
  }else{
    
    # Create a palette with the specified number of colors
    rbPal <- brewer.pal(12, "Paired")
    rbPal[11]<-"#C32876"
    my_labels <- as.character(month.abb)
    data$month<-format(data[,1],'%m')
    
    for(i in names(data[-1])){
      
      data_boxplot <- ggplot(data, aes(x = month, y = .data[[i]], color = month)) +
        theme_bw() +
        labs(title = paste("Average monthly for", i), x = "Month") +
        theme(plot.title = element_text(size=8))+
        scale_colour_manual(values = rbPal) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
          panel.spacing = unit(0.5, "cm"),
          axis.text = element_text(size = 5),
          axis.title=element_text(size=7)
        ) +
        geom_boxplot(fill = "#e0dede", outlier.shape = NA,size=.5) +
        labs(y = var.name) +
        scale_x_discrete(labels = my_labels)+
        theme(legend.key.size = unit(0.2, 'cm'))+
        ylim(min(box.stats[[i]]$stats),max(box.stats[[i]]$stats)) +
        guides(fill="none", color="none")  # Hide the color legend
      if(!is.na(family)){
        data_boxplot <- data_boxplot + theme(text=element_text(family=family))
      }
      # Save the plot as a JPEG fil
      ggsave(filename = paste0(output_dir,var.name," ", i, "_bxpl.jpeg"), data_boxplot , width = 10, height = 10, units = "cm", dpi = resol)
    }
  }
}
  