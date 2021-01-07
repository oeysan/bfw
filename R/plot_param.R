#' @title Plot Param
#' @description Create a density plot with parameter values
#' @param data MCMC data to plot
#' @param param parameter of interest
#' @param ROPE plot ROPE values, Default: FALSE
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#495054", "#e3e8ea")
#' @param font.type font type used for visualizations, Default: 'serif'
#' @param font.size font size, Default: 4.5
#' @param rope.line size of ROPE lien, Default: -0.2
#' @param rope.tick distance to ROPE tick, Default: -0.1
#' @param rope.label distance to ROPE label, Default: -0.35
#' @param line.size overall line size, Default: 0.5
#' @param dens.zero.col colour of line indicating zero, Default: 'black'
#' @param dens.mean.col colour of line indicating mean value, Default: 'white'
#' @param dens.median.col colour of line indicating median value, Default: 'white'
#' @param dens.mode.col colour of line indicating mode value, Default: 'black'
#' @param dens.rope.col colour of line indicating ROPE value, Default: 'black'
#' @param scale scale x and y axis, Default: FALSE
#' @param y.limits vector of y limits, Default: NULL
#' @param y.breaks vector of y breaks, Default: NULL
#' @param x.limits = vector of x limits, Default: NULL
#' @param x.breaks = vector of x breaks, Default: NULL
#' @param plot.title = title of plot, Default: NULL
#' @return Density plot of parameter values
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{join}},\code{\link[dplyr]{select}},\code{\link[dplyr]{slice}},\code{\link[dplyr]{filter}}
#'  \code{\link[stats]{approxfun}}
#'  \code{\link[ggplot2]{aes}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{geom_density}},\code{\link[ggplot2]{geom_polygon}},\code{\link[ggplot2]{geom_segment}},\code{\link[ggplot2]{geom_label}},\code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{ggplot_build}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}}
#' @rdname PlotParam
#' @export 
#' @importFrom stats approx

PlotParam <- function (data,
                       param,
                       ROPE = FALSE,
                       monochrome = TRUE,
                       plot.colors = c("#495054", "#e3e8ea"),
                       font.type = "serif",   
                       font.size = 4.5,
                       rope.line = -0.2,
                       rope.tick = -0.1,
                       rope.label = -0.35,
                       line.size = 0.5,
                       dens.zero.col = "black",
                       dens.mean.col = "white",
                       dens.median.col = "white",
                       dens.mode.col = "black",
                       dens.rope.col = "black",
                       scale = FALSE,
                       y.limits = NULL,
                       y.breaks = NULL,
                       x.limits = NULL,
                       x.breaks = NULL,
                       plot.title = NULL) {
  
  # Check if ggplots is installed
  if (!requireNamespace("ggplot2", quietly = TRUE) |
      !requireNamespace("magrittr", quietly = TRUE) |
      !requireNamespace("dplyr", quietly = TRUE)) {
    stop("Packages \"ggplot2\", \"magrittr\", and \"dplyr\" 
         are needed for this function to work. Please install them.",
         call. = FALSE)
  }
  
  # Define import elements                     
  `%>%` <- magrittr::`%>%`
  mutate <- dplyr::mutate
  group_by <- dplyr::group_by
  left_join <- dplyr::left_join
  select <- dplyr::select
  slice <- dplyr::slice
  approx <- stats::approx
  filter <- dplyr::filter
  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank
  element_line <- ggplot2::element_line
  element_text <- ggplot2::element_text
  geom_density <- ggplot2::geom_density
  geom_polygon <- ggplot2::geom_polygon
  geom_segment <- ggplot2::geom_segment
  geom_text <- ggplot2::geom_text
  ggplot <- ggplot2::ggplot
  ggplot_build <- ggplot2::ggplot_build
  scale_x_continuous <- ggplot2::scale_x_continuous
  scale_y_continuous <- ggplot2::scale_y_continuous
  theme <- ggplot2::theme  
  HDIhi <- NULL
  HDIlo <- NULL
  Mean <- NULL
  Median <- NULL
  Mode <- NULL
  ROPEhi <- NULL
  ROPEin <- NULL
  ROPElo <- NULL
  ROPEmax <- NULL
  ROPEmin <- NULL
  dens.mean <- NULL
  dens.median <- NULL
  dens.mode <- NULL
  dens.rope.max <- NULL
  dens.rope.min <- NULL
  dens.zero <- NULL
  var <- NULL
  x <- NULL
  y <- NULL
  if (is.null(plot.title)) plot.title <- summary$param
  
  data.matrix <- data$matrix.MCMC
  param.summary.col <- MultiGrep(param, 
                                 rownames(data$summary.MCMC), 
                                 value = FALSE)
  param.mcmc.col <- MultiGrep(param, colnames(data.matrix), value = FALSE)
  raw.data <- data.frame(data.matrix[, param.mcmc.col])
  colnames(raw.data) <- "var"
  raw.data$param <- param
  
  summary <- as.data.frame(t(data$summary.MCMC[ param.summary.col , ]))
  use.cols <- c("Mean", 
                "Median", 
                "Mode", 
                "HDIlo", 
                "HDIhi", 
                "ROPEmin", 
                "ROPEmax", 
                "ROPElo", 
                "ROPEhi", 
                "ROPEin")
  summary <- summary[colnames(summary) %in% use.cols]
  summary$Min <- min(raw.data$var)
  summary$Max <- max(raw.data$var)
  summary$param <- param
  
  dens.data <- suppressMessages(
    ggplot_build(ggplot(raw.data, aes(x=var, colour=param)) + 
                   geom_density())$data[[1]] %>%
      mutate(param = summary$param) %>%
      left_join(summary) %>%
      select(y, 
             x, Mean, 
             Median, 
             Mode, 
             HDIlo, 
             HDIhi, 
             ROPEmin, 
             ROPEmax, 
             ROPElo, 
             ROPEhi, 
             ROPEin, 
             Max, 
             Min, 
             param) %>%
      mutate(dens.zero = approx(x, y, xout = 0)[[2]],
             dens.mean = approx(x, y, xout = Mean)[[2]],
             dens.median = approx(x, y, xout = Median)[[2]],
             dens.mode = approx(x, y, xout = Mode)[[2]],
             dens.rope.min = approx(x, y, xout = ROPEmin)[[2]],
             dens.rope.max = approx(x, y, xout = ROPEmax)[[2]]) %>%
      select(-y, -x) %>%
      slice(1)
  )
  
  
  dens.data[is.na(dens.data)] <- dens.data$dens.mode
  
  # Create area for hdi in density plot
  hdi.ribbon <- suppressMessages(
    ggplot_build(ggplot(raw.data, aes(x=var, colour=param)) + 
                   geom_density())$data[[1]] %>%
      mutate(param = summary$param) %>%
      left_join(dens.data) %>%
      group_by(param) %>%
      filter(x >= HDIlo & x <= HDIhi) %>%
      select(param, x, y)
  )
  
  # Add zero distribution to ribbon
  hdi.ribbon <- rbind(data.frame(param = summary$param, 
                                 x = summary$HDIlo, y = 0),
                      as.data.frame(hdi.ribbon), 
                      data.frame(param = summary$param, 
                                 x = summary$HDIhi, y = 0))
  
  if (ROPE) {
    Min <- dens.data$ROPEmin
    Max <- dens.data$ROPEmax
  } else {
    Min <- 0
    Max <- 0
  }
  
  dens.data$Min <- min(dens.data$Min,Min)
  dens.data$Max <- max(dens.data$Max,Max)
  if (dens.data$dens.rope.min 
      >= dens.data$Min) dens.data$dens.rope.min <- 0
  if (dens.data$dens.rope.max 
      >= dens.data$Max) dens.data$dens.rope.max <- 0
  font.size.pts <- font.size/0.352777778
  
  
  # round(seq(dens.data$Min, dens.data$Max, by = 0.05),1)
  
  plot <- ggplot() +
    ggplot2::labs(title = plot.title , x = "Parameter value", y = "Density") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(size=.1, color="grey"),
          text=element_text(family=font.type, size = font.size.pts),
          legend.position="none") +
    geom_density(data = raw.data, 
                 aes(x = var), 
                 fill = "grey") +
    geom_polygon(data = hdi.ribbon, 
                 aes(x = x, y = y), 
                 fill = "grey42") +
    geom_segment(data = dens.data, 
                 aes(x = 0, xend = 0, y = 0, yend = dens.zero),
                 color = dens.zero.col , 
                 linetype = "dotted", 
                 size = line.size) +
    geom_segment(data = dens.data, 
                 aes(x = Mean, xend = Mean, y = 0, yend = dens.mean),
                 color =dens.mean.col , 
                 linetype = "solid", 
                 size = line.size) +
    geom_segment(data = dens.data, 
                 aes(x = Median, xend = Median, y = 0, yend = dens.median),
                 color = dens.median.col , 
                 linetype = "dashed", 
                 size = line.size) +
    geom_segment(data = dens.data, 
                 aes(x = Mode, xend = Mode, y = 0, yend = dens.mode),
                 color = dens.mode.col , 
                 linetype = "solid", 
                 size = line.size)
  
  
  if (scale) {
    
    plot <- plot +
      scale_x_continuous(limits = y.limits, breaks = y.breaks) +
      scale_y_continuous(limits = x.limits, breaks = x.breaks)
    
  }
  
  if (ROPE) {
    
    plot <- plot + 
      geom_segment(data = dens.data, 
                   aes(x = ROPEmin, 
                       xend = ROPEmin, 
                       y = 0, 
                       yend = dens.rope.min),
                   colour = dens.rope.col , 
                   linetype = "dashed", 
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = ROPEmax, 
                       xend = ROPEmax, 
                       y = 0, 
                       yend = dens.rope.max),
                   colour = dens.rope.col , 
                   linetype = "dashed", 
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = Min, 
                       xend = ROPEmin, 
                       y = rope.line, 
                       yend = rope.line),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = ROPEmin, 
                       xend = ROPEmax, 
                       y = rope.line , 
                       yend = rope.line),
                   colour = dens.rope.col,
                   linetype = "dashed",
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = ROPEmax, 
                       xend = Max, 
                       y = rope.line, 
                       yend = rope.line),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = ROPEmin, 
                       xend = ROPEmin, 
                       y = rope.line,
                       yend = rope.tick),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) + 
      geom_segment(data = dens.data, 
                   aes(x = ROPEmax, 
                       xend = ROPEmax, 
                       y = rope.line, 
                       yend = rope.tick),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) + 
      geom_segment(data = dens.data, 
                   aes(x = Min, 
                       xend = Min, 
                       y = rope.line, 
                       yend = rope.tick),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) +
      geom_segment(data = dens.data, 
                   aes(x = Max, 
                       xend = Max, 
                       y = rope.line, 
                       yend = rope.tick),
                   colour = dens.rope.col, 
                   linetype = "dashed", 
                   size = line.size) +
      geom_text(data = dens.data, 
                aes(x = mean(c(Min, ROPEmin)), 
                    y = rope.label, 
                    label = sprintf("%0.2f%%", ROPElo)), 
                family = font.type , size = font.size) +
      geom_text(data = dens.data, 
                aes(x = mean(c(ROPEmin, ROPEmax)),
                    y = rope.label, label = sprintf("%0.2f%%", ROPEin)), 
                family = font.type , size = font.size) +
      geom_text(data = dens.data, 
                aes(x = mean(c(ROPEmax, Max)), 
                    y = rope.label, label = sprintf("%0.2f%%", ROPEhi)), 
                family = font.type , size = font.size)
  }
  
  return (plot)
  
}