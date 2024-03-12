
#' Title
#'
#' @param data 
#' @param station_info_df 
#' @param start.date 
#' @param end.date 
#' @param time.step 
#' @param remove.zero 
#' @param plot 
#' @param resol 
#' @param var.name
#' @param language 
#'
#' @return
#' @export
#'
#' @examples
outlierDETEC <- function(data,station_info_df, start.date = "01-01-1982", end.date = "17-12-2023", time.step = "day", remove.zero = TRUE, 
                        plot = TRUE, resol = 400, var.name = "Precipitation (mm)", output_dir = "./", xcluster = TRUE,
                        language = c("en","es"), unit="mm"){
  
  # Check if the first column of the data is of class "Date" and add a date column if not
  if(class(data[,1]) == "Date"){
    print("The first column of your data frame are dates")
  } else {
    data[,1] <- seq(as.Date(start.date, format = "%d-%m-%Y"), as.Date(end.date, format = "%d-%m-%Y"), by = time.step)
  }
  
  iden.outlier <- function(Main.dat) {
    if (length(Main.dat$group) == 0) {
      out <- NA
    } else {
      x <- Main.dat$out
      y <- Main.dat$group
      z <- Main.dat$stats
      
      M.dat1 <- cbind(x, y)
      M.dat <- split(M.dat1[ ,1], M.dat1[ ,2])
      vect.ident <- matrix(NA, 12, 2)
      exis<-names(M.dat)
      
      for(k in exis){
        out.limin <- which(M.dat[[k]] <= z[1, as.numeric(k)])
        out.limax <- which(M.dat[[k]] >= z[5, as.numeric(k)])
        
        if(length(out.limin) != 0 & length(out.limax) != 0){
          vect.ident[as.numeric(k),1] <- mean(M.dat[[k]][out.limin], na.rm = TRUE)
          vect.ident[as.numeric(k),2] <- mean(M.dat[[k]][out.limax], na.rm = TRUE)  
        } else if(length(out.limin) == 0 & length(out.limax) != 0){
          vect.ident[as.numeric(k),2] <- mean(M.dat[[k]][out.limax], na.rm = TRUE)  
        } else {
          vect.ident[as.numeric(k),1] <- mean(M.dat[[k]][out.limin], na.rm = TRUE)
        }
      }
    }
    return(vect.ident)
  }
  
  #titles according to language
  if(language=="en"){
    main_plot <- "Outlier detection for"
    legend_plot <- c("1"="lower","2"="upper")
    stat_plot <- "Median "
    count_plot <- "Count"
    my_labels <- as.character(month.abb)
  }else if( language=="es"){
    main_plot <- "Detección de atípicos para"
    legend_plot <- c("1"="Mínimo","2"="Máximo")
    stat_plot <- "Mediana "
    count_plot <- "Conteo"
    my_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun","Jul",
                   "Ago", "Sep", "Oct", "Nov", "Dic")
  }
  
  # Optionally remove zero values from the data
  if(remove.zero){
    data.1 <- as.data.frame(apply(data[,-1], 2, function(x) replace(x, x %in% 0, NA)))
    data.1$dates <- data[,1]
    data <- data.1[,c("dates",colnames(data.1[,-c(ncol(data.1))]))]
    
  } else {
  }
  
  box.stats <- apply(data[,-1], 2,function(x, y) boxplot(x~y, outline=F,plot = FALSE,range=1.5), y = format(data[,1],'%m'))
  id.out <- lapply(box.stats, iden.outlier)
  count.out<-lapply(box.stats, function(x) table(x$group))
  
  
  
  if(!plot){
  }else{
    # Create a palette with the specified number of colors
    rbPal <- brewer.pal(12, "Paired")
    rbPal[11]<-"#C32876"
    
    data$month<-format(data[,1],'%m')
    for(i in names(box.stats)){
      
      count_barplot <- ggplot(as.data.frame(count.out[[i]]), aes(x = Var1, y = Freq, fill = factor(Var1))) +
        geom_bar(stat = "identity",alpha=0.6) +
        ylab(count_plot) +
        scale_fill_manual(values = rbPal) +
        theme_bw() +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          panel.spacing = unit(0.5, "cm"),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()
        ) +
        guides(fill="none",color = "none")+
        ylim(0, 1.2*max(as.data.frame(count.out[[i]])[2]))
      
      data_boxplot <- ggplot(data, aes(x = month, y = .data[[i]], color = month)) +
        theme_bw() +
        labs(title = paste(main_plot, i), x = "") +
        scale_colour_manual(values = rbPal) +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
          panel.spacing = unit(0.5, "cm"),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_blank()
        ) +
        geom_boxplot(fill = "#e0dede", outlier.shape = NA,size=.5) +
        labs(y = var.name) +
        ylim(min(box.stats[[i]]$stats),max(box.stats[[i]]$stats)) +
        guides(fill="none",color = "none")  # Hide the color legend
      
      median_barplot <- ggplot(reshape2::melt(id.out[[i]]), aes(x = as.factor(Var1), y = value, fill = as.factor(Var2))) +
        geom_bar(stat = "identity",alpha=0.6, position = "dodge") +
        ylab(paste0(stat_plot,"(", unit, ")")) +
        scale_fill_manual(values = c("1"='#8c510a',"2"='#80cdc1'),labels = legend_plot) +
        theme_bw() +
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          panel.spacing = unit(0.5, "cm"),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 8)
        ) +
        theme(legend.position = c(0.9, 0.8),legend.title = element_blank(),
              legend.text = element_text( size=7))+
        scale_x_discrete(labels = my_labels)+
        theme(legend.key.size = unit(0.2, 'cm'))+
        ylim(0, 2*max(as.data.frame(id.out[[i]])[2]))
      
      # Print both plots side by side
    p<-grid.arrange(data_boxplot, count_barplot,median_barplot,ncol = 1,heights = c(10,5,5))
    dev.off()
      
      # Save the plot as a JPEG file
      ggsave(filename = paste0(output_dir,var.name,"_", i, "_outl.jpeg"), p , width = 12, height = 15, units = "cm", dpi = resol)
    }
  }
  
  if(!xcluster){
  }else{
    
    df_list <- lapply(id.out, as.data.frame)
    combined_dataframe <- as.data.frame(t(do.call(cbind, df_list)))
    combined_dataframe$code <- sub("\\..*", "", row.names(combined_dataframe))
    combined_dataframe$var <- sub("^[^.]+\\.", "", row.names(combined_dataframe))
    mer.dataframe<-merge(combined_dataframe, station_info_df, by = "code", all.x=TRUE) %>%
      select(code,var,cluster,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12)
    
    
    q.95 <-aggregate(.~var+cluster, data =as.data.frame(mer.dataframe[mer.dataframe$var=="V1",-c(1)]), quantile,probs = 0.95, na.rm= TRUE, na.action = na.pass)
    ncluster<-length(unique(q.95$cluster))
    
    if (sum(is.na(mer.dataframe[mer.dataframe$var=="V2",-c(1,2,3)]))!=dim(mer.dataframe[mer.dataframe$var=="V2",-c(1,2,3)])[1]*dim(mer.dataframe[mer.dataframe$var=="V2",-c(1,2,3)])[2]) {
      
      q.05 <-aggregate(.~cluster+var, data =mer.dataframe[mer.dataframe$var=="V2",-c(1)], quantile,probs = 0.05, na.rm= TRUE,na.action = na.pass)
      
      plot_df.1 <- data.frame(value = unlist(q.05[, -c(1,2)]))
      plot_df.2 <- data.frame(value = unlist(q.95[,  -c(1,2)]))
      plot_df<-rbind(plot_df.1,plot_df.2)
      plot_df$var<-rep(c("V1","V2"),each=ncluster*12)
    }else{
      plot_df <- data.frame(value = unlist(q.95[,  -c(1,2)]))
      plot_df$var<-rep(c("V1"),each=ncluster*12)
    }
    
    month.f<-substr(row.names(plot_df), start = 2, stop = nchar(row.names(plot_df))-1)
    plot_df$month.f <-rep(1:12,each=ncluster)
    plot_df$cluster<-rep(seq(1, ncluster, 1), 12)
    
    p1 <- ggplot(plot_df, aes(x = as.factor(month.f),y=value, group = var, fill=var)) +
      geom_bar(stat = "identity",alpha=0.6, position = "dodge") +
      labs(title = paste0("Outliers - ",var.name),
           x = "Month",
           y = var.name) +  # Legend title
      scale_fill_manual(values = c("V2" = "darkcyan", "V1" = "darkorchid4"),labels = c("V1" ="Upper outliers","V2" ="lower outliers")) +
      facet_wrap(~cluster, scales = "free_y", ncol = 2) +
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        panel.spacing = unit(0.5, "cm"),
      ) +
      theme(legend.position = c(0.9, 1.1),legend.title = element_blank(),legend.key=element_blank(),
            legend.text = element_text( size=7), legend.key.size = unit(0.1, 'cm'))+
      scale_x_discrete(limits = as.character(1:12), breaks = as.character(1:12))
    
    # Save the plot as a JPEG file
    ggsave(filename = paste0(output_dir,var.name,"_cluster_outliers.jpeg"), p1 , width = 12, height = 13, units = "cm", dpi = resol)
  }
  return(count.out)
  
  
}


  