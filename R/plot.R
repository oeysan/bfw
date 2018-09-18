#' @title Plot Data
#' @description Plot data as violin plot visualizing density, box plots to display HDI, whiskers to display standard deviation
#' @param data data to plot data from
#' @param data.type define what kind of data is being used, Default: 'Mean'
#' @param ... further arguments passed to or from other methods
#' @rdname PlotData
#' @export
PlotData <- function(data, data.type = "Mean", ... ) {
  
  # Trim and set data.type to lower case
  data.type <- Trim(tolower(data.type))  
  
  # If data is nominal
  if (data.type == "nominal") {
    
    Plot <- PlotNominal(data = data,...)
    
  } 
  # If data is metric
  else if (data.type == "mean") {
    
    Plot <- PlotMean(data = data, ...)
    
  } 
  # If data is circlize
  else if (data.type == "circlize") {
    
    Plot <- PlotCirclize(...)
    
  } 
  # Else stop function
  else {
    
    stop("This type of plot does not exist... yet.")
    
  }
}
