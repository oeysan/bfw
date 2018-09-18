#' @title Plot Nominal
#' @description Create a nominal plot
#' @param data MCMC data to plot
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#CCCCCC", "#DEDEDE")
#' @param font.type font type used for visualizations, Default: 'serif'
#' @param bar.dodge distance between within bar plots, Default: 0.6
#' @param bar.alpha transparency for bar plot, Default: 0.7
#' @param bar.width width of bar plot, Default: 0.4
#' @param bar.extras.dodge dodge of error bar and label, Default: 0
#' @param bar.border color of the bar border, Default: 'black'
#' @param bar.label logical, indicating whether or not to show bar labels, Default: TRUE
#' @param bar.error logical, indicating whether or not to show error bars, Default: TRUE
#' @param use.cutoff logical, indicating whether or not to use a cutoff for keeping plots, Default: FALSE
#' @param diff.cutoff if using a cutoff, determine the percentage that expected and observed values should differ, Default: 1
#' @param q.items which variables should be used in the plot. Defaults to all , Default: NULL
#' @seealso 
#'  \code{\link[ggplot2]{aes}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{geom_crossbar}},\code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{scale_manual}},\code{\link[ggplot2]{theme}}
#' @export 
#' @importFrom grDevices colorRampPalette
PlotNominal <- function (data, 
                         monochrome = TRUE,
                         plot.colors = c("#CCCCCC", "#DEDEDE"),
                         font.type = "serif",
                         bar.dodge = 0.6,
                         bar.alpha = 0.7,
                         bar.width = 0.4,
                         bar.extras.dodge = 0,
                         bar.border = "black",
                         bar.label = FALSE,
                         bar.error = TRUE,
                         use.cutoff = FALSE,
                         diff.cutoff = 1,
                         q.items = NULL) {

  # Check if ggplots is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The \"ggplot2\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  if (tolower(data$name$model.type) != "nominal") {
    stop("This is not a bfw nominal summary")
  }
  
  # Define ggplot2 elements
  aes <- ggplot2::aes
  element_blank <- ggplot2::element_blank
  element_text <- ggplot2::element_text  
  geom_errorbar <- ggplot2::geom_errorbar
  ggplot <- ggplot2::ggplot
  scale_fill_manual <- ggplot2::scale_fill_manual
  theme <- ggplot2::theme
  theme_bw <- ggplot2::theme_bw
  element_line <- ggplot2::element_line
  ylab <- ggplot2::ylab
  geom_bar <- ggplot2::geom_bar
  position_dodge <- ggplot2::position_dodge
  geom_label <- ggplot2::geom_label
  facet_grid <- ggplot2::facet_grid
  ggtitle <- ggplot2::ggtitle
  labs <- ggplot2::labs
  
  data.set <- data$name.list$data.set
  sum <- data$summary.MCMC
  name.list <- data$name.list$job.names
  q <- length(data$data.list$q.levels)
  q.items <- if (!length(q.items)) seq(q) else ParseNumber(q.items)
  
  expected.var <- unlist(lapply(q.items, function(i) {
    apply(combn(q, i), 2, function (x) {
      paste0(paste0("e",x,collapse=""),"p")
    })
  }))
  
  observed.var <- unlist(lapply(q.items, function(i) {
    apply(combn(q, i), 2, function (x) {
      paste0(paste0("o",x,collapse=""),"p")
    })
  }))
  
  
  Plots <- lapply(1:length(expected.var), function (i) {
    
    
    q.items <- ParseNumber(expected.var[[i]])
    q <- length(data$data.list$q.levels[q.items])
    x.names <- colnames(data$data.list$x)[q.items]
    x.cols <- paste0("X",seq(q))
    x.colors <- if (q>1) data$data.list$q.levels[q.items[2]] else data$data.list$q.levels[q.items[1]]
    
    expected <- sum[grepl(paste0("\\b",expected.var[[i]],"\\b"), rownames(sum)),][,c(3,5:6)] 
    expected <- apply( expected, 2, function (x) round(x, digits = 2))
    expected <- cbind(expand.grid(name.list[[1]][q.items]), 
                      expected , 
                      type= rep("Expected",nrow(expected))
    )
    
    observed <- sum[grepl(paste0("\\b",observed.var[[i]],"\\b"), rownames(sum)),][,c(3,5:6)] 
    observed <- apply( observed, 2, function (x) round(x, digits = 2))
    observed <- cbind(expand.grid(name.list[[1]][q.items]), 
                      observed , 
                      type= rep("Observed",nrow(observed))
    )
    
    test.diff <- cbind(expected, observed)
    test.diff <- all(apply(test.diff, 1, function (x) {
      i <- grep("Mode", names(x))
      x <- as.numeric(x[i[1]]) - as.numeric(x[i[2]])
      (x<diff.cutoff)
    }))
    
    if (!test.diff | !use.cutoff) {
      
      # Combine expected and predicted data into plot.data
      plot.data <- rbind(expected, observed)
      colnames(plot.data) <- c(x.cols,"Mode","LB","UB","Type")
      
      plot.title <- paste0(x.names[1] , 
                           if (q>1) " by " , 
                           paste(tail(x.names,q-1), collapse = " and "),
                           sprintf(" [%s]", data.set)
      )
      
      # Add label identifier for expected/observed
      plot.data$label <- rep(seq(2), 
                             each = sum(plot.data$Type=="Expected"))
      
      # Select (monochrome) colors
      if (monochrome) {
        plot.colors <- grDevices::colorRampPalette(plot.colors)(x.colors)
      } else { 
        plot.colors <- DistinctColors(1:x.colors) 
      }
      
      
      LB <- plot.data$LB
      Mode <- plot.data$Mode
      Type <- plot.data$Type
      UB <- plot.data$UB
      label <- plot.data$label
      X1 <- plot.data$X1
      
      Plot <- ggplot( plot.data , aes( x = X1 , y = Mode , fill = factor(Type), ymax=UB, ymin=LB) ) +
        ggtitle(plot.title)  +  
        theme_bw() +
        theme(text=element_text(family=font.type),
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.spacing = unit(.08, "lines"),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              legend.position="bottom",
              strip.background = element_blank(),
              legend.key = element_blank(), 
              plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
        ylab("Percent") +
        labs(fill = "") +
        geom_bar(stat = "identity", 
                 position = position_dodge(width = bar.dodge), 
                 colour = bar.border, 
                 width = bar.width,
                 alpha = bar.alpha) +
                 {if (bar.error) geom_errorbar(aes(ymin = LB, ymax = UB, width=0.1 , group = factor(label), colour = factor(Type)),
                                               position = position_dodge(bar.dodge+bar.extras.dodge),
                                               colour = "black")} +
                                               {if (bar.label) geom_label( aes(y=Mode, label=Mode, group = factor(label)), 
                                                                           colour = "black",
                                                                           fill = "white",
                                                                           position = position_dodge(bar.dodge+bar.extras.dodge), 
                                                                           family = font.type,
                                                                           label.padding = unit(0.5, "lines"),
                                                                           show.legend = FALSE)} +
        scale_fill_manual(values = c(plot.colors)) 
      
      if (q==2) {
        Plot <- Plot + facet_grid(. ~ X2)
      } else if (q>2) {
        facet.y <- paste0("X",q)
        facet.x <- paste(paste0( "X", seq(q-1)[-1] ), collapse="+")
        facet.eval <- sprintf("facet_grid(%s)", paste(facet.y,facet.x, sep="~"))
        Plot <- Plot + eval(parse(text=facet.eval))
      }  
      
      return(Plot)
      
    }
    
  })
  
  # Remove empty plots
  Plots <- Filter(length, Plots)
}