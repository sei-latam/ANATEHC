#' Title
#'
#' @param TS 
#' @param agreggate 
#' @param type 
#' @param save 
#'
#' @return
#' @export
#'
#' @examples
#' 
plotTS.interact <- function(TS = , agreggate = "DMY", type = "", save = TRUE, 
                            FUN.ARG = mean, na.act = TRUE){
  
  nvar <- length(TS)
  if (nvar == 0){
    stop('The list must contain at least one time series variable')
  }
  
  # All series must be time series class
  if(class(TS)[1] != "xts"){
  } else{
    warning(paste('The variable(s) are not a time series', dQuote(c("xts")) ,'class'))
  }
  
  if(!is.null(FUN.AG)){
    
    if(agreggate == "DM" | agreggate == "M"){
      var.mon <- xts::apply.monthly(TS, FUN = FUN.ARG, na.rm = na.act)
    } else if (agreggate == "DMY"){
      var.mon <- xts::apply.monthly(TS, FUN = FUN.ARG, na.rm = na.act)
      var.year <- xts::apply.yearly(TS, FUN = FUN.ARG, na.rm = na.act)
    } else if (agreggate == "DY" | agreggate == "Y"){
      var.year <- xts::apply.yearly(TS, FUN = FUN.ARG, na.rm = na.act)
    } else{
      stop("There are not aggregation time step")
    }
    
  }else{
    cat("There are not aggregation function, so just plotting in time step of TS")
  }
  
  # Tipos de graficos 
  
  if(grepl("D", agreggate, fixed = TRUE)){
    plot.1 <- dygraphs::dygraph(TS, main = main) %>%
      dygraphs::dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE)
  }
  if(grepl("M", agreggate, fixed = TRUE)){
    plot.2 <- dygraphs::dygraph(var.mon, main = main, height = 300, width = "100%") %>%
      dygraphs::dyHighlight(highlightCircleSize = 5, 
                            highlightSeriesBackgroundAlpha = 0.2,
                            hideOnMouseOut = FALSE) %>%
      dygraphs::dyRangeSelector(height = 25) %>%
      dygraphs::dyLegend(show = "follow", width = 210, hideOnMouseOut = FALSE) 
  }
  if(grepl("Y", agreggate, fixed = TRUE)){
    plot.3 <- dygraphs::dygraph(var.year, main = main, height = 300, width = "100%") %>%
      dygraphs::dyHighlight(highlightCircleSize = 5, 
                            highlightSeriesBackgroundAlpha = 0.2,
                            hideOnMouseOut = FALSE) %>%
      dygraphs::dyRangeSelector(height = 25) %>%
      dygraphs::dyLegend(show = "follow", width = 210, hideOnMouseOut = FALSE) 
  }
  
  if(agreggate == "DMY"){
    plot <- list(plot.1, plot.2, plot.3)
    plot <- htmltools::browsable(htmltools::tagList(plot))
  }else if(agreggate == "MY"){
    plot <- list(plot.2, plot.3)
    plot <- htmltools::browsable(htmltools::tagList(plot))
  }else if(agreggate == "DY"){
    plot <- list(plot.1, plot.3)
    plot <- htmltools::browsable(htmltools::tagList(plot))
  }else if(nchar(agreggate)==1){
    if(agreggate == "D"){
      plot <- htmltools::browsable(htmltools::tagList(plot.1))
    }else if(agreggate == "M"){
      plot <- htmltools::browsable(htmltools::tagList(plot.2))
    }else{
      plot <- htmltools::browsable(htmltools::tagList(plot.3))
    }
  }else{
    print("There are no timestep defined for plotting")
  }  
    
    
  
  
  
}
  