#' @title Diagnose MCMC 
#' @description MCMC convergence diagnostics
#' @param data.MCMC MCMC chains to diagnose
#' @param par.name parameter to analyze
#' @param job.names names of all parameters in analysis, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param project.name name of project, Default: 'Project'
#' @param project.dir define where to save data, Default: 'Results/'
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#495054", "#e3e8ea")
#' @param graphic.type type of graphics to use (e.g., pdf, png, ps), Default: 'pptx'
#' @param plot.size size of plot, Default: '15,10'
#' @param scaling scale size of plot, Default: 100
#' @param plot.aspect aspect of plot, Default: NULL
#' @param vector.graphic logical, indicating whether or not visualizations should be vector or raster graphics, Default: TRUE
#' @param point.size point size used for visualizations, Default: 15
#' @param font.type font type used for visualizations, Default: 'serif'
#' @param one.file logical, indicating whether or not visualizations should be placed in one or several files, Default: TRUE
#' @param ppi define pixel per inch used for visualizations, Default: 300
#' @param units define unit of length used for visualizations, Default: 'in'
#' @param layout define a layout size for visualizations, Default: 'pw'
#' @param layout.inverse logical, indicating whether or not to inverse layout (e.g., landscape) , Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @return list of diagnostic plots
#' @seealso 
#'  \code{\link[grDevices]{dev.new}},\code{\link[grDevices]{colorRampPalette}},\code{\link[grDevices]{recordPlot}},\code{\link[grDevices]{graphics.off}},\code{\link[grDevices]{dev.list}},\code{\link[grDevices]{dev.off}}
#'  \code{\link[graphics]{par}},\code{\link[graphics]{layout}},\code{\link[graphics]{plot.new}},\code{\link[graphics]{matplot}},\code{\link[graphics]{abline}},\code{\link[graphics]{text}},\code{\link[graphics]{points}},\code{\link[graphics]{mtext}}
#'  \code{\link[coda]{traceplot}},\code{\link[coda]{gelman.plot}},\code{\link[coda]{effectiveSize}}
#'  \code{\link[stats]{sd}},\code{\link[stats]{acf}},\code{\link[stats]{density}}
#' @rdname DiagMCMC
#' @export 
#' @importFrom grDevices dev.new colorRampPalette recordPlot graphics.off dev.list dev.off
#' @importFrom graphics par layout plot.new matplot abline text points mtext
#' @importFrom coda traceplot gelman.plot effectiveSize
#' @importFrom stats sd acf density
DiagMCMC <- function(data.MCMC, 
                    par.name, 
                    job.names, 
                    job.group, 
                    project.name,
                    project.dir,
                    save.data = FALSE,
                    credible.region = 0.95,
                    monochrome = TRUE,
                    plot.colors = c("#495054", "#e3e8ea"),
                    graphic.type = "pptx",
                    plot.size = "15,10",
                    scaling = 100,
                    plot.aspect = NULL,
                    vector.graphic = FALSE,
                    point.size = 15,
                    font.type = "serif",
                    one.file = TRUE,
                    ppi = 300,
                    units = "in",
                    layout = "pw",
                    layout.inverse = FALSE,
                    ...
) {
  

  # Trim and split plot size
  plot.size <- as.numeric(TrimSplit(plot.size))
  
  # Use width / height if aspect ratio is not defined
  if (is.null(plot.aspect)) plot.aspect <- plot.size[[1]] / plot.size[[2]]
  # Extract width of page
  page.width <- Layout(layout,layout.inverse)[1]
  # Extract height of page
  page.height <- Layout(layout,layout.inverse)[2]
  # Aspect ratio of page
  page.aspect <- page.width / page.height
  # If aspect ratio of page is creater than the aspect rataio of plot adjust width factor
  width.factor <- if (page.aspect > plot.aspect) plot.aspect / page.aspect else 1
  # If aspect ratio of page is creater than the aspect rataio of plot adjust height factor
  height.factor <- if (page.aspect > plot.aspect) 1 else page.aspect / plot.aspect
  # Define width of plot based on scaling and page width and width factor
  plot.width <- ( scaling / 100 ) * ( page.width * width.factor )
  # Define height of plot based on scaling and page heighbt and height factor
  plot.height <- ( scaling / 100 ) * ( page.height * height.factor )
  
  # Open new graphics device
  grDevices::dev.new(width=plot.width,
                     height=plot.height,
                     noRStudioGD = TRUE,
                     res=ppi,
                     units="in")
                       
  plot.colors <- if (monochrome) grDevices::colorRampPalette(plot.colors)(4) else DistinctColors(1:4)
  graphics::par(mar = 0.5 + c(3, 4, 1, 0),
                oma = 0.1 + c(0, 0, 2, 0),
                mgp = c(2.25, 0.7, 0),
                cex.lab = 1.5,
                par(family = font.type))
  graphics::layout(matrix(1:4, nrow = 2))
  
  # Traceplot
  coda::traceplot(data.MCMC[, c(par.name)], 
                  main = "", 
                  ylab = "Param. Value", 
                  col = plot.colors)
  
  # Shrink factor
  gelman <- try(coda::gelman.plot(data.MCMC[, c(par.name)], 
                                  main = "", 
                                  auto.layout = FALSE, 
                                  col = plot.colors))
  
  # if it runs, gelman.plot returns a list with finite shrink values
  if (class(gelman) == "try-error") {
    graphics::plot.new()
    print(paste0("Warning:coda::gelman.plot fails for ", par.name))
  } else {
    if (class(gelman) == "list" & !is.finite(gelman$shrink[1])) {
      graphics::plot.new()
      print(paste0("Warning:coda::gelman.plot fails for ", par.name))
    }
  }
  n.chain <- length(data.MCMC)
  diag.data <- do.call(cbind,data.MCMC[, par.name][1:n.chain])
  ESS <- coda::effectiveSize(rowMeans(diag.data)) # Effective sample size
  MCSE <- stats::sd(rowMeans(diag.data)) / sqrt(ESS) # Monte Carlo standard error  
 
  # Autocorrelation
  x <- apply(diag.data, 2 , function (x) stats::acf(x, plot = FALSE)$lag)  
  y <- apply(diag.data, 2 , function (x) stats::acf(x, plot = FALSE)$acf)
   
  graphics::matplot(x, 
                    y, 
                    type = "o", 
                    pch = 20, 
                    col = plot.colors,
                    ylim = c(0, 1), 
                    main = "", 
                    xlab = "Lag", 
                    ylab = "Autocorrelation")
  graphics::abline(h = 0, lty = "dashed")
  graphics::text(x = max(x), 
                 y = max(y), 
                 adj = c(1.0, 1.0), 
                 cex = 1.25, 
                 labels = paste("ESS = ", round(ESS, 1)))
    
  # Density
  x <- apply(diag.data, 2 , function (x) stats::density(x)$x)  
  y <- apply(diag.data, 2 , function (x) stats::density(x)$y)
  hdi.interval <- apply(diag.data, 2 , function (x) ComputeHDI(x,  credible.region) ) 
  graphics::matplot(x, 
                    y, 
                    type = "l", 
                    col = plot.colors, 
                    main = "", 
                    xlab = "Param. Value", 
                    ylab = "Density")
  graphics::abline(h = 0)
  graphics::points(hdi.interval[1, ], 
                   rep(0, n.chain), 
                   col = plot.colors, 
                   pch = "|")
  graphics::points(hdi.interval[2, ], 
                   rep(0, n.chain), 
                   col = plot.colors, 
                   pch = "|")
  graphics::text(mean(hdi.interval), 
                 0, 
                 "95% HDI", 
                 adj = c(0.5, -0.2))
  graphics::text(max(x), 
                 max(y), 
                 adj = c(1.0, 1.0), 
                 cex = 1.25, 
                 paste("MCSE = \n", signif(MCSE, 3)))
  graphics::mtext(text = AddNames(par.name, job.names, job.group), 
                  outer = TRUE, 
                  adj = c(0.5, 0.5), 
                  cex = 2.0)
  
  if (save.data) {
      # Record plot
      recorded.plot <- grDevices::recordPlot()
       
      return (recorded.plot) 
  }
}
