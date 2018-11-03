#' @title Plot Mean
#' @description Create a (repeated) mean plot
#' @param data MCMC data to plot
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#495054", "#e3e8ea")
#' @param font.type font type used for visualizations, Default: 'serif'
#' @param run.repeated logical, indicating whether or not to use repeated measures plot, Default: FALSE
#' @param run.split logical, indicating whether or not to use split violin plot and compare distribution between groups, Default: FALSE
#' @param y.split logical, indicating whether or not to split within (TRUE) or between groups, Default: FALSE
#' @param ribbon.plot logical, indicating whether or not to use ribbon plot for HDI, Default: TRUE
#' @param y.text label on y axis, Default: 'Score'
#' @param x.text label on x axis, Default: NULL
#' @param remove.x logical, indicating whether or not to show x.axis information, Default: FALSE
#' @seealso 
#'  \code{\link[ggplot2]{ggproto}},
#'  \code{\link[ggplot2]{ggplot2-ggproto}},
#'  \code{\link[ggplot2]{aes}},
#'  \code{\link[ggplot2]{margin}},
#'  \code{\link[ggplot2]{geom_boxplot}},
#'  \code{\link[ggplot2]{geom_crossbar}},
#'  \code{\link[ggplot2]{geom_path}},
#'  \code{\link[ggplot2]{geom_ribbon}},
#'  \code{\link[ggplot2]{geom_violin}},
#'  \code{\link[ggplot2]{ggplot}},
#'  \code{\link[ggplot2]{scale_manual}},
#'  \code{\link[ggplot2]{scale_x_discrete}},
#'  \code{\link[ggplot2]{theme}},
#'  \code{\link[ggplot2]{layer}},
#'  \code{\link[ggplot2]{labs}}
#'  \code{\link[plyr]{arrange}},
#'  \code{\link[plyr]{rbind.fill}}
#'  \code{\link[scales]{zero_range}}
#'  \code{\link[grid]{grid.grob}},
#'  \code{\link[grid]{grobName}},
#'  \code{\link[grid]{unit}}
#'  \code{\link[stats]{approxfun}}
#'  \code{\link[grDevices]{colorRamp}}
#' @rdname PlotMean
#' @export 
#' @importFrom grid grobTree grobName unit
#' @importFrom stats approxfun
#' @importFrom grDevices colorRampPalette

PlotMean <- function (data,
                      monochrome = TRUE,
                      plot.colors = c("#495054", "#e3e8ea"),
                      font.type = "serif",                     
                      run.repeated = FALSE,
                      run.split = FALSE,
                      y.split = FALSE ,
                      ribbon.plot = TRUE,
                      y.text = "Score",
                      x.text = NULL,
                      remove.x = FALSE) {
                      
  # Check if ggplots is installed
  if (!requireNamespace("ggplot2", quietly = TRUE) |
      !requireNamespace("scales", quietly = TRUE) |
      !requireNamespace("plyr", quietly = TRUE) |
      !requireNamespace("grid", quietly = TRUE)) {
    stop("Packages \"ggplot2\", \"grid\", \"plyr\" and \"scales\" are needed for this function to work. Please install them.",
         call. = FALSE)
  }
  
  # Define ggplot2 elements
  ggproto <- ggplot2::ggproto
  GeomViolin <- ggplot2::GeomViolin
  GeomPath <- ggplot2::GeomPath
  GeomPolygon <- ggplot2::GeomPolygon
  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank
  element_line <- ggplot2::element_line
  element_rect <- ggplot2::element_rect
  element_text <- ggplot2::element_text
  geom_boxplot <- ggplot2::geom_boxplot
  geom_errorbar <- ggplot2::geom_errorbar
  geom_line <- ggplot2::geom_line
  geom_ribbon <- ggplot2::geom_ribbon
  geom_violin <- ggplot2::geom_violin
  ggplot <- ggplot2::ggplot
  scale_fill_manual <- ggplot2::scale_fill_manual
  scale_x_discrete <- ggplot2::scale_x_discrete
  theme <- ggplot2::theme
  
  # Split violin plot for ggplot2
  # Based on Jan Gleixner (@jan-glx) and Wouter van der Bijl (@Axeman)
  # https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-and-add-information
  GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, draw_quantiles = NULL, ...) {
    data <- transform( data,
                       xminv = data$x - data$violinwidth * (data$x - data$xmin),
                       xmaxv = data$x + data$violinwidth * (data$xmax - data$x) )
    groups <- data[1,"group"]
    
    x.var <- if ( groups %% 2 == 1 ) data$xminv else data$xmaxv
    y.var <- if ( groups %% 2 == 1 ) data$y else -data$y
    
    n.data <- plyr::arrange(transform( data, x = x.var), y.var)
    n.data <- rbind(n.data[1, ], n.data, n.data[nrow(n.data), ], n.data[1, ])
    n.data[c(1,nrow(n.data)-1,nrow(n.data)), "x"] <- round(n.data[1, "x"])
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- cQuantile(data, draw_quantiles, split = TRUE, groups = groups)
      aesthetics <- data[rep(1, nrow(quantiles)),
                         setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      grob <- grid::grobTree(GeomPolygon$draw_panel(n.data, ...), quantile_grob)
    } else {
      grob <- GeomPolygon$draw_panel(n.data, ...)
    }
    
    grob$name <- grid::grobName(grob, "geom_splitviolin")
    return (grob)
    
  } )
  
  cQuantile <- function (data, draw_quantiles, split = FALSE, groups = NULL) {
    dens <- cumsum(data$density)/sum(data$density)
    ecdf <- stats::approxfun(dens, data$y)
    ys <- ecdf(draw_quantiles)
    violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
    violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
    violin.xs <- (stats::approxfun(data$y, data$x))(ys)
    if (groups %% 2 == 0) {
      data.frame(x = Interleave(violin.xs, violin.xmaxvs),
                 y = rep(ys, each = 2), group = rep(ys, each = 2))
    } else {
      data.frame(x = Interleave(violin.xminvs, violin.xs),
                 y = rep(ys, each = 2), group = rep(ys, each = 2))
    }
  }
  
  geom_splitviolin <- function (mapping = NULL,
                                data = NULL,
                                stat = "ydensity",
                                position = "identity",
                                draw_quantiles = NULL,
                                trim = TRUE,
                                scale = "area",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                ...
  ) {
    
    ggplot2::layer(data = data,
                   mapping = mapping,
                   stat = stat,
                   geom = GeomSplitViolin,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(trim = trim,
                                 scale = scale,
                                 draw_quantiles = draw_quantiles,
                                 na.rm = na.rm,
                                 ...
                   ) )
  }
  
  # Extract save name
  project.name <- data$name.list$project.name
  
  # Get variables definitions from storage
  summary.MCMC <- data$summary.MCMC
  n <- data$data.list$n
  q <- data$data.list$q
  y <- data$data.list$y
  job.names <- data$name.list$job.names
  job.title <- data$name.list$job.title
  y.names <- data$name.list$y.names
  x.names <- data$name.list$x.names
  x.li <- data$name.list$x.li
  
  # y length
  y.length <- length(y.names)
  # by sequence
  x.sequence <- unlist(lapply(x.li , length))
  # Items in per y sequence
  y.sequence <- seq(1+sum(x.sequence))
  # y and by sequence
  k.sequence <- c(1,x.sequence)
  # Start position of y for normal sequences
  y.start <- seq(1, length(k.sequence)*y.length, length(k.sequence))
  # Position of y and by in normal sequences
  normal.position <- unlist(lapply(y.start, function  (i) rep(i:(i+length(x.sequence)), k.sequence)))
  # Position of y and by in repeated sequences
  repeated.position <- rep(y.sequence,y.length)
  # By groups
  x.groups <- rep(rep(1:(length(k.sequence))-1, k.sequence), y.length)
  # Subnames
  sub.names <- as.character(unlist(x.li))
  # y groups
  y.groups <- rep( seq(y.length), each = max(y.sequence) )
  # Select number of variables per group
  sub.groups <- unlist(lapply(1:(length(k.sequence)*y.length), function (i) seq(sum(normal.position %in% i))))
  # Position of y in sequence
  y.position <- seq(1,q,max(y.sequence))
  # Sequences of data to plot
  plot.sequence <- if (run.repeated) repeated.position else normal.position
  # Lists of variables to plot
  plot.variables <- lapply(1:max(plot.sequence), function (i) matrix(which(plot.sequence %in% i) ) )
  # Create data frame with group indices and Bayesian statistics
  plot.data <- do.call(rbind,lapply(1:q, function (i) {
    
    x.names <- if (!i %in% y.position) x.names[x.groups[i]] else NA
    sub.names <- if (!i %in% y.position) sub.names[(repeated.position-1)[i]] else NA
    
    data.frame(
      y.names = y.names[y.groups[i]],
      x.names = x.names,
      sub.names = sub.names,
      y.groups = y.groups[i],
      by.groups = x.groups[i],
      sub.groups = sub.groups[i],
      k = i,
      n = n[i],
      # Bayesian stastistics
      sd.lower.max = summary.MCMC[i, 5] - summary.MCMC[i+q, 6],
      sd.lower.mode = summary.MCMC[i, 3] - summary.MCMC[i+q, 3],
      sd.lower.min = summary.MCMC[i, 6] - summary.MCMC[i+q, 5],
      mean.lower.hdi = summary.MCMC[i, 5],
      mean.mode = summary.MCMC[i, 3],
      mean.upper.hdi = summary.MCMC[i, 6],
      sd.upper.min = summary.MCMC[i, 5] + summary.MCMC[i+q, 5],
      sd.upper.mode = summary.MCMC[i, 3] + summary.MCMC[i+q, 3],
      sd.upper.max = summary.MCMC[i, 6] + summary.MCMC[i+q, 6]
    )
    
  }))
  
  # Create list of number of split violin plots and variables per plot
  ## Repeated split plots
  if (run.split & run.repeated) {
    plot.variables <- do.call(cbind, lapply(1:y.length, function (i) {
      m <- lapply(1:length(x.li), function (j) {
        t(combn(plot.data[ plot.data$y.groups == i & plot.data$by.groups == j  , "k"],2))
      } )
      plyr::rbind.fill(m)
    } ) )
    plot.variables <- lapply(1:nrow(plot.variables), function (i) matrix(plot.variables[i,], ncol=2, byrow=TRUE))
    # horizontal split plots (variables per group)
  } else if (run.split & !y.split) {
    plot.variables <- unlist ( lapply(1:y.length, function (i) {
      lapply(1:length(x.names), function (j) {
        t(combn(plot.data[ plot.data$y.groups == i & plot.data$by.groups == j  , "k"],2))
      } )
    } ), recursive=FALSE)
    # vertical split plots (same variable per y)
  } else if (run.split & y.split) {
    plot.variables <- lapply(y.sequence, function (i) {
      t(combn(seq(i,q,max(y.sequence)),2))
    } )
  }
  
  # number of plots
  n.plots <- length(plot.variables)
  
  # run all plots
  run.plots <- lapply(1:n.plots, function (i) {
    
    # i in n plots
    plot.variables <- plot.variables[[i]]
    # observations
    n <- plot.data[ c(plot.variables) , "n"]
    # Names
    y.names <- plot.data[ c(plot.variables) , "y.names"]
    x.names <- plot.data[ c(plot.variables) , "x.names"]
    sub.names <- plot.data[ c(plot.variables) , "sub.names"]
    
    # Define names for the various combination of plots (split, repeated, normal)
    ## Beware! Hasty and messy coding
    if ( (run.split & !y.split & !run.repeated) | !run.repeated ) job.names <- y.names
    
    if (run.repeated & run.split) {
      plot.title <- paste(job.title, "by", x.names)
      label.groups <- y.names
    } else if (all(is.na(sub.names))) {
      plot.title <- job.title
      label.groups <- unique(y.names)
    } else if (all(sub.names[1] == sub.names) & run.repeated) {
      plot.title <- sprintf("%s by %s [%s]",job.title,x.names,sub.names)
      label.groups <- unique(y.names)
    } else if (all(sub.names[1] == sub.names)) {
      plot.title <- sprintf("%s [%s]", x.names,sub.names)
      label.groups <- unique(y.names)
    } else {
      plot.title <- paste(job.names, "by", x.names)
      label.groups <- unique(sub.names)
    }
    
    # define colors
    n.colors <- if (run.repeated & run.split) length(unique(sub.names)) else length(label.groups)
    plot.colors <- if (monochrome) grDevices::colorRampPalette(plot.colors)(n.colors) else DistinctColors(1:n.colors)
    
    # create xtick names
    if (run.split) {
      x.ticks <- unique(plot.data[,1])
    } else {
      group <- if (run.repeated) unique(plot.data[,1]) else label.groups
      x.ticks <- lapply(1:length(group), function (i) {
        bquote(atop(.(as.character(group[i])),"("*italic("n")==.(n[i])*")"))
      })
    }
    
    # group names
    if (run.repeated & run.split) {
      label.groups <- unique(sub.names)
      n <- rep(unique(n), length(label.groups))
    }
    label.groups <- lapply(1:length(label.groups), function (i) {
      bquote(.(as.character(label.groups[i]))~"("*italic("n")==.(n[i])*")")
    })
    
    # box data sizes and positions
    if (run.split) {
      size.prop <- matrix(prop.table(plot.data[ c(plot.variables) , "n"  ]),ncol=2)
      if (nrow(plot.variables)==1) size.prop <- size.prop / 3
      if (nrow(plot.variables)==2) size.prop <- size.prop / 1.5
      size <- size.prop[,1]
      size2 <- size.prop[,2]
      x.pos <- seq(nrow(plot.variables))-(size/2)
      x.pos2 <- seq(nrow(plot.variables))+(size2/2)
    } else {
      size <- prop.table(plot.data[ c(plot.variables) , "n"  ])
      if (nrow(plot.variables)==1) size <- size / 3
      if (nrow(plot.variables)==2) size <- size / 1.5
      x.pos <- seq(nrow(plot.variables))
    }
    
    # more group namings
    if (run.repeated & !run.split) {
      groups <- plot.data[c(plot.variables), "y.names" ]
    } else if (run.repeated) {
      groups <- plot.data[c(plot.variables), "sub.names" ]
    } else {
      groups <- c(plot.variables)
    }
    
    # Violin data
    violin.data <- data.frame(
      x = rep(seq(nrow(plot.variables)), each = nrow(y)),
      y = c(y[, c(plot.variables)]),
      groups = rep(groups, each = nrow(y))
    )
    
    # Box data (Bayesian statistics)
    plot.data <- do.call(cbind,lapply(1:ncol(plot.variables), function (i) {
      t <- plot.data[ c(plot.variables[,i]) , match("sd.lower.max",names(plot.data)):ncol(plot.data)]
      if (i>1) colnames(t) <- paste0(colnames(t),i)
      return (t)
    } ) )
    
    # Create factors and remove NA
    violin.data$x <- factor(violin.data$x)
    violin.data$groups <- factor(violin.data$groups)
    violin.data <- violin.data[complete.cases(violin.data), ]
    
    sd.lower.max <- violin.data$sd.lower.max
    sd.lower.mode <- violin.data$sd.lower.mode
    sd.lower.min <- violin.data$sd.lower.min
    mean.lower.hdi <- violin.data$mean.lower.hdi
    mean.mode <- violin.data$mean.mode
    mean.upper.hdi <- violin.data$mean.upper.hdi
    sd.upper.min <- violin.data$sd.upper.min
    sd.upper.mode <- violin.data$sd.upper.mode
    sd.upper.max <- violin.data$sd.upper.max
    sd.lower.max2 <- violin.data$sd.lower.max2
    sd.lower.mode2 <- violin.data$sd.lower.mode2
    sd.lower.min2 <- violin.data$sd.lower.min2
    mean.lower.hdi2 <- violin.data$mean.lower.hdi2
    mean.mode2 <- violin.data$mean.mode2
    mean.upper.hdi2 <- violin.data$mean.upper.hdi2
    sd.upper.min2 <- violin.data$sd.upper.min2
    sd.upper.mode2 <- violin.data$sd.upper.mode2
    sd.upper.max2 <- violin.data$sd.upper.max2
    x <- violin.data$x
    
    # Create plot
    plot <- suppressWarnings(ggplot(plot.data) +
    {if (!run.split) geom_violin(data=violin.data, aes(x, y, fill=groups), draw_quantiles = 0.5, alpha = 0.9, trim=FALSE, lwd=0.1)}+
    {if (run.split) geom_splitviolin(data=violin.data, aes(x, y, fill = groups), alpha = 0.9, trim=FALSE, lwd=0.1)}+
    {if (ribbon.plot & run.repeated) geom_ribbon(aes(x=x.pos, ymin = mean.lower.hdi, ymax = mean.upper.hdi), alpha=0.3)}+
    {if (ribbon.plot & run.repeated & run.split) geom_ribbon(aes(x=x.pos2, ymin = mean.lower.hdi2, ymax = mean.upper.hdi2), alpha=0.3)}+
    {if (run.repeated) geom_line(aes(x=x.pos, y=mean.mode), lwd=0.1 )}+
    {if (run.repeated & run.split) geom_line(aes(x=x.pos2, y=mean.mode2), lwd=0.1 )}+
      geom_boxplot(aes(x=x.pos, 
                       ymin = sd.lower.min, 
                       lower = mean.lower.hdi, 
                       middle = mean.mode, 
                       upper = mean.upper.hdi, 
                       ymax = sd.upper.min, 
                       width = size), 
                   stat = "identity", 
                   lwd=0.1)+
      geom_errorbar(aes(x=x.pos, ymin = sd.upper.min, ymax = sd.upper.max), lwd=0.05, width = 0.05)+
      geom_errorbar(aes(x=x.pos, ymin = sd.upper.mode, ymax = sd.upper.mode), lwd=0.05, width = 0.1)+
      geom_errorbar(aes(x=x.pos, ymin = sd.lower.min, ymax = sd.lower.max), lwd=0.05, width = 0.05)+
      geom_errorbar(aes(x=x.pos, ymin = sd.lower.mode, ymax = sd.lower.mode), lwd=0.05, width = 0.1)+
      {if (run.split) geom_boxplot(aes(x=x.pos2, 
                                       ymin = sd.lower.min2, 
                                       lower = mean.lower.hdi2, 
                                       middle = mean.mode2, 
                                       upper = mean.upper.hdi2, 
                                       ymax = sd.upper.min2, 
                                       width = size2), 
                                   stat = "identity", 
                                   lwd=0.1)}+
                                   {if (run.split) geom_errorbar(aes(x=x.pos2, ymin = sd.upper.min2, ymax = sd.upper.max2), lwd=0.05, width = 0.05)}+
                                   {if (run.split) geom_errorbar(aes(x=x.pos2, ymin = sd.upper.mode2, ymax = sd.upper.mode2), lwd=0.05, width = 0.1)}+
                                   {if (run.split) geom_errorbar(aes(x=x.pos2, ymin = sd.lower.min2, ymax = sd.lower.max2), lwd=0.05, width = 0.05)}+
                                   {if (run.split) geom_errorbar(aes(x=x.pos2, ymin = sd.lower.mode2, ymax = sd.lower.mode2), lwd=0.05, width = 0.1)}+
                                   {if (!run.split | run.repeated & run.split) scale_x_discrete(labels=x.ticks)}+
      scale_fill_manual(labels = label.groups , values = plot.colors)+
      ggplot2::labs(title = plot.title , x = x.text, y = y.text)+
      theme(legend.position="top",
            legend.key.size = grid::unit(0.75, 'lines'),
            legend.key = element_rect(size = 1),
            legend.title=element_blank(),
            plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
            text = element_text(family=font.type),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(lineheight = 9),
            panel.grid.major.y = element_line( size=.01, color="lightgrey"),
            axis.title.x=element_blank())+
            {if (!run.split) theme(legend.position = "none")}+
            {if (run.split & !run.repeated | remove.x) theme(axis.text.x=element_blank(),
                                                  axis.ticks.x=element_blank())}
    )                                           
    
  })
  
}