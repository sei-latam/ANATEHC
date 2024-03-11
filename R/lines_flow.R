rm(list=ls())
cat("\014")

# Load required libraries
library(lubridate)
library(openxlsx)
library(tidyverse)
library(hydroTSM)
library(trend)
library(plotrix)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

base.path <-"C:/Users/CamiloSanabria/OneDrive - SEI/CAEM_SEI/00_SEI/10_DB-IDEAM/Hidrologia/Q/"
stations.path <-"C:/Users/CamiloSanabria/SEI/Consorcio CAEM-SEI FA - Documentos/General/4. Informacion/SIG/Regiones_Priorizadas/"
var.name <- "Flow (cms)"
resol<-400
output_dir<-"./"

time_series_df<- as.data.frame(read.csv(paste0(base.path,"QMED_data_COC.csv"), sep = ","))
colnames(time_series_df)[colnames(time_series_df) == 'Fecha'] <- 'date'
start.d <- "01-01-1982" ## m%d%y
end.d <- as.Date(time_series_df[[1]][length(time_series_df[[1]])], format="%m/%d/%Y")
dates<- seq(as.Date(start.d, format = "%d-%m-%Y"), as.Date(end.d, format = "%d-%m-%Y"), by = "day")
time_series_df$date <- dates

threshold<- length(dates)-length(dates)*.3
na_counts <- colSums(is.na(time_series_df))
cols_to_keep <- which(na_counts <= threshold)
df_filtered <- time_series_df[, cols_to_keep, drop = FALSE]
time_series_df<-df_filtered

station_info_df<- as.data.frame(read.xlsx(paste0(stations.path,"Caribe_Occidental/ListadoEstaciones_CNE_IDEAM_2023-05-24_V.xlsx"), sheet =2))
colnames(station_info_df)[colnames(station_info_df) == c('CODIGO','nombre')] <- c('code', 'name')
colnames(station_info_df)[colnames(station_info_df) == 'latitud'] <- 'latitude'
station_info_df[[1]] <- paste0("X", station_info_df[[1]])

remove.zero=FALSE
  # Optionally remove zero values from the data
  if(remove.zero){
    data.1 <- as.data.frame(apply(time_series_df[,-1], 2, function(x) replace(x, x %in% 0, NA)))
    data.1$dates <- time_series_df[,1]
    time_series_df <- data.1[,c("dates",colnames(data.1[,-c(ncol(data.1))]))]
  } else {
  }
  
  
  time_series_df[[1]] <- as.Date(time_series_df[[1]])
  
  time_series_df<-time_series_df %>%
    mutate(Month = month(date), Year = year(date))
  
  df_long <- time_series_df %>%
    gather(key = "Station", value = "Value", -date, -Month, -Year)
  
  summary_stats <- df_long %>%
    group_by(Month, Station) %>%
    summarise(
      Min = min(Value, na.rm = TRUE),
      Mean = mean(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE)
    )
  
  mer.df<- as.data.frame(merge(summary_stats,station_info_df,by.x = "Station", by.y = "code", all.x = TRUE))
  
  breaks = seq(min(mer.df$altitud), max(mer.df$altitud) + 500, by = 500)
  labels <- c(paste(breaks[-length(breaks)], "-", breaks[-1]), paste(">", breaks[length(breaks)]))[-length(breaks)]
  mer.df$group <- cut(mer.df$altitud, breaks = breaks, include.lowest = TRUE, labels = labels)
  
  mer.df2<-mer.df[,c("Station","Month","group","Mean","Min","Max")]
  
  
  test<-aggregate(.~group+Month,data=mer.df2[,-1],mean,na.rm=TRUE)
  
  p1<-ggplot()+
    
    geom_line(data=test,aes(x=factor(Month),y=Mean,group=group,color = "Mean"),linewidth=0.5)+
    geom_line(data=test,aes(x=factor(Month),y=Max,group=group, color = "Max"),linewidth=0.2)+
    geom_line(data=test,aes(x=factor(Month),y=Min,group=group,color="Min"),linewidth=0.2)+
    facet_wrap(~group, scales = "free_y",ncol = 2)+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=.5))+
    labs(title = "Monthly mean Flow",x="Month") +
    ylab(paste0(var.name)) +
    theme(panel.spacing = unit(0.5, "cm"), 
          axis.text.y = element_text(size = 8))+
    theme(legend.position = "bottom",legend.key.size = unit(.3, 'cm'))+
    scale_color_manual(values = c("Mean"="blue", "Min"="green","Max"="red")) +
    guides(color=guide_legend(title=NULL))
  
  p1
  
  # Save the plot as a JPEG file
  ggsave(file.path(paste0("./COC/COC",var.name, "_flowelv.jpeg")), p1, width = 12, height = 15, units = "cm", dpi = resol)
  write.csv(mer.df2, file = "C:/Users/CamiloSanabria/OneDrive - SEI/CAEM_SEI/00_SEI/10_DB-IDEAM/Hidrologia/Qelev_COC.csv")
  