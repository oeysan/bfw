#' @title Circlize Plot
#' @description Create a circlize plot
#' @param data data for circlize plot
#' @param category.spacing spacing between category items , Default: 1.25
#' @param category.inset inset of category items form plot , Default: c(-0.5, 0)
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#CCCCCC", "#DEDEDE")
#' @param font.type font type used for visualizations, Default: 'serif'
#' @seealso
#' \code{\link[grDevices]{dev}},
#' \code{\link[grDevices]{recordPlot}}
#' \code{\link[graphics]{legend}}
#' \code{\link[circlize]{circos.par}},
#' \code{\link[circlize]{chordDiagram}},
#' \code{\link[circlize]{circos.trackPlotRegion}},
#' \code{\link[circlize]{circos.clear}}
#' @rdname PlotCirclize
#' @export
#' @importFrom grDevices dev.new recordPlot dev.off
#' @importFrom graphics legend
PlotCirclize <- function (data,
                          category.spacing = 1.2,
                          category.inset = c(-0.4, 0),
                          monochrome = TRUE,
                          plot.colors = c("#CCCCCC", "#DEDEDE"),
                          font.type = "serif") {
  
  # Check if circlize is installed
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("The \"circlize\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Clear circlize
  circlize::circos.clear()
  
  # Fetch category, items and selects
  category <- data$category
  category.items <- data$category.items
  category.selects <- data$category.selects
  
  # Set 0 as missing
  category.selects[category.selects == 0] <- NA
  # Remove missing
  category.selects <- category.selects[rowSums(is.na(category.selects)) != ncol(category.selects), ]
  
  # Find connections between categories
  category.combn <- lapply(1:nrow(category.selects), function (i) {
    
    x <- which(!is.na(category.selects[i,]))
    if (length(x)>1) {
      mat <- t(combn(x,2))
    } else {
      mat <- matrix(c(x,x),1,2)
    }
    
    as.data.frame(mat)
    
  })
  
  # create a two-column matrix of combinations
  if (!requireNamespace("data.table", quietly = TRUE)) {
    category.combn <- as.matrix(do.call(rbind,category.combn))
  } else {
    category.combn <- as.matrix(data.table::rbindlist(category.combn))
  }
  
  # Sort connections
  category.combn <- category.combn[rev(order(category.combn[,1])),]
  
  # Find unique categories
  unique.categories <- sort(unique(c(category.combn)))
  
  # Select (monochrome) colors
  if (monochrome) {
    circlize.colors <- grDevices::colorRampPalette(plot.colors)(length(unique.categories))
  } else {
    circlize.colors <- DistinctColors(1:length(unique.categories))
  }
  
  # Add plot
  grDevices::dev.new(width=15,
                     height=10,
                     res=300,
                     noRStudioGD = TRUE,
                     units="in")
  
  # Define plot parameters
  par(mar = c(0, 0, 0, 15),
      xpd = TRUE,
      pty = "s",
      family = font.type)
  
  # Create connections between variables
  gap.degree <- do.call("c", lapply(table(unique.categories), function(i) {
    c(rep(2, i - 1), 8)
  }))
  
  circlize::circos.par(gap.degree = gap.degree)
  
  circlize::chordDiagram(
    as.data.frame(category.combn),
    order = unique.categories ,
    grid.col = circlize.colors,
    directional = FALSE,
    annotationTrack = "grid",
    preAllocateTracks = list(list(track.height = 0.02))
  )
  
  # Add names/numbers
  circlize::circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
    xlim = circlize::get.cell.meta.data("xlim")
    ylim = circlize::get.cell.meta.data("ylim")
    sector.index = circlize::get.cell.meta.data("sector.index")
    circlize::circos.text(mean(xlim), mean(ylim), sector.index, col = "black",
                          cex = 1, niceFacing = TRUE,
                          facing = "inside", adj = c(0.5, -1.2))
  }, bg.border = NA)
  
  # Legend text
  legend.items <- list(lapply(seq_along(category.items), function(i) {
    sprintf("%s (%s)",
            category.items[i],
            max(table(category.selects[,i]),0)
    )
  }))
  
  # Legend title
  legend.title <- list(
    bquote(
      bold(.(sprintf("%s (%s)", category, length(which(!is.na(unlist(category.selects)))))))
    )
  )
  
  # Create legend
  graphics::legend("right",
                   title = do.call(expression, unlist(legend.title)),
                   legend = unlist(legend.items),
                   cex = 1,
                   y.intersp = category.spacing,
                   inset = category.inset,
                   bty = "n"
  )
  
  # Clear circlize
  circlize::circos.clear()
  
  # Record graphics device
  p <- grDevices::recordPlot()
  grDevices::dev.off()
  return (p)
  
}