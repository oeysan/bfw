#' @title Parse Plot
#' @description Display and/or save plots
#' @param plot.data a list of plots
#' @param project.dir define where to save data, Default: 'Results/'
#' @param project.name define name of project, Default: 'FileName(name="Print")'
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
#' @param graphic.type type of graphics to use (e.g., pdf, png, ps), Default: 'pdf'
#' @param plot.size size of plot, Default: '15,10'
#' @param scaling scale size of plot, Default: 100
#' @param plot.aspect aspect of plot, Default: NULL
#' @param vector.graphic logical, indicating whether or not visualizations should be vector or raster graphics, Default: FALSE
#' @param point.size point size used for visualizations, Default: 12
#' @param font.type font type used for visualizations, Default: 'serif'
#' @param one.file logical, indicating whether or not visualizations should be placed in one or several files, Default: TRUE
#' @param ppi define pixel per inch used for visualizations, Default: 300
#' @param units define unit of length used for visualizations, Default: 'in'
#' @param layout define a layout size for visualizations, Default: 'a4'
#' @param layout.inverse logical, indicating whether or not to inverse layout (e.g., landscape) , Default: FALSE
#' @param return.files logical, indicating whether or not to return saved file names
#' @param ... further arguments passed to or from other methods
#' @examples
#' # Create three plots
#' plot.data <- lapply(1:3, function (i) {
#'   # Open new device
#'   grDevices::dev.new()
#'   # Print plot
#'   plot(1:i)
#'   # Record plot
#'   p <- grDevices::recordPlot()
#'   # Turn off graphics device drive
#'   grDevices::dev.off()
#'   return (p)
#' } )
#'
#' # Print plots
#' ParsePlot(plot.data)
#' \donttest{
#' # Save plots as png with a4 layout and return file names
#' project.dir <- tempdir()
#' project.name <- FileName(name="Testing-Plot")
#' ParsePlot(plot.data,
#'           project.dir = project.dir,
#'           project.name = project.name,
#'           graphic.type = "png",
#'           save.data = TRUE,
#'           layout = "a4",
#'           return.files = TRUE
#' )
#' # [1] "\\Temp/Project-Testing-Plot01-1528833217.png"
#' # [2] "\\Temp/Project-Testing-Plot02-1528833217.png"
#' # [3] "\\Temp/Project-Testing-Plot03-1528833217.png"
#' # Save plots as single PowerPoint (default) and return file names
#' project.dir <- tempdir()
#' project.name <- FileName(name="Testing-Plot")
#' ParsePlot(plot.data,
#'           project.dir = project.dir,
#'           project.name = project.name,
#'           vector.graphic = FALSE,
#'           graphic.type = "pptx",
#'           layout = "pw",
#'           save.data = TRUE,
#'           return.files = TRUE
#' )
#' # [1] "\\Temp/Project-Testing-Plot-1528833342.pptx"
#' }
#' @seealso 
#'  \code{\link[grDevices]{dev}},\code{\link[grDevices]{png}},\code{\link[grDevices]{ps.options}},\code{\link[grDevices]{recordPlot}}
#'  \code{\link[utils]{head}}
#'  \code{\link[png]{readPNG}}
#'  \code{\link[graphics]{par}},\code{\link[graphics]{plot}},\code{\link[graphics]{rasterImage}}
#'  \code{\link[officer]{read_pptx}},\code{\link[officer]{add_slide}},\code{\link[officer]{ph_with}}
#'  \code{\link[rvg]{dml}}
#' @rdname ParsePlot
#' @export 
#' @importFrom grDevices dev.new png dev.off setEPS recordPlot
#' @importFrom utils tail
#' @importFrom graphics par plot rasterImage

ParsePlot <- function (plot.data,
                       project.dir = "Results/",
                       project.name = FileName(name="Print"),
                       graphic.type = "pdf",
                       plot.size = "15,10",
                       scaling = 100,
                       plot.aspect = NULL,
                       save.data = FALSE,
                       vector.graphic = FALSE,
                       point.size = 12,
                       font.type = "serif",
                       one.file = TRUE,
                       ppi = 300,
                       units = "in",
                       layout = "a4",
                       layout.inverse = FALSE,
                       return.files = FALSE,
                       ...) {
          
  # If single plot add plot to list
  if (any(inherits(plot.data, "ggplot"), 
        inherits(plot.data, "recordedplot"))) plot.data <- list(plot.data)

  # Check if plot
  if ( !any(inherits(plot.data[[1]], "ggplot"), 
        inherits(plot.data[[1]], "recordedplot")) ) stop("This is not a plot.")
  
  # Count number of plots
  n.plots <- length(plot.data)
  
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
  
  # Set start time
  plot.start.time  <- Sys.time()
  
  # Print plots save data is not selected
  if (!save.data) {
    
    cat("\nPrinting images. Please wait.\n")
    invisible(lapply(seq(n.plots), function (i) {
      # Open new graphics device
      grDevices::dev.new(width=plot.width,
                         height=plot.height,
                         noRStudioGD = TRUE,
                         res=ppi,
                         units="in")
      # Print plot                   
      print(plot.data[[i]])
      
      ETA(plot.start.time , i , n.plots)
      
    }))
    
    # Else parse plots for saving
  } else {
    
    # Trim and lowercase graphic type
    graphic.type <- Trim(tolower(graphic.type))
    if (graphic.type == "ppt") graphic.type <- "pptx"
    if (graphic.type == "jpg") graphic.type <- "jpeg"
    
    # Check if 'png', officer' and 'rvg' are installed
    if (graphic.type == "pptx" & vector.graphic &
        !requireNamespace("officer", quietly = TRUE) &
        !requireNamespace("rvg", quietly = TRUE)) {
      warning("\nThe function need packages 'officer' and 'rvg' to create PowerPoint with vector graphics.\n",
              "Defaults to 'pdf'.\n")    
      graphic.type <- "pdf" 
    } else if (graphic.type == "pptx" & vector.graphic &
               !requireNamespace("rvg", quietly = TRUE)) {
      warning("\nThe function need package 'rvg' to create PowerPoint with vector graphics.\n",
              "Defaults to PowerPoint with raster graphics.\n")     
      vector.graphic <- FALSE
    } else if (graphic.type == "pptx" & 
               !vector.graphic & !requireNamespace("officer", quietly = TRUE)) {
      warning("\nThe function need packages 'officer' to create PowerPoint.\n",
              "Defaults to 'pdf'.\n")    
      graphic.type <- "pdf" 
    } else if (!vector.graphic & !requireNamespace("png", quietly = TRUE) &
               (graphic.type == "pdf" | graphic.type == "postscript" )) {
      warning("\nThe function need the 'png' package to create raster graphics for pdf and postscript \n",
              "Defaults to 'vector'.\n")    
      vector.graphic <- TRUE
    }
    
    # Set graphics device driver (if eps/ps set postscript else use file name extension)
    dev.type <- if (graphic.type == "eps" | graphic.type == "ps") "postscript" else graphic.type
    
    # If necessary add trailing slash to project directory
    if ( utils::tail(TrimSplit(project.dir,""),1) != "/") {
      project.dir <- paste0(project.dir,"/")
    }
    
    # Create directories
    if (!dir.exists(project.dir)) {
      dir.create(project.dir,recursive = TRUE)
    }
    
    # Decide whether to use singular or plural in plot name
    if (one.file & ( dev.type == "postscript" |
                     dev.type == "pdf" |
                     dev.type == "pptx" ) ) {
      plot.type <- ""
    } else {
      plot.type <- "%02d"
    }
    
    # split file name
    names.vector <- TrimSplit(project.name,"-")
    # Number of elements in file name
    n.names <- length(names.vector)
    # Check if file name has unix time stamp
    time.stamp  <- nchar(ParseNumber(utils::tail(names.vector,1))) > 9
    
    # If file name has unix timestamp prepend stamp or append to name
    ## Add file extension
    if (time.stamp) {
      project.name <- paste0(
        paste0(names.vector[-n.names],collapse="-"),
        plot.type,
        "-", names.vector[n.names],
        ".", graphic.type
      )
    } else {
      project.name <- paste0(project.name, plot.type , "." , graphic.type)
    }
    
    # if multiple files and PowerPoint, change %02d to regular numerics
    if (!one.file & dev.type == "pptx") {
      padded.vector <- PadVector(seq(n.plots))
      project.name <- unlist(lapply(padded.vector, function (i) gsub("%02d",i,project.name) ) )
    }
    
    # Create final file name
    file.name <- paste0(project.dir,project.name)
    
    # Create raster graphics for postscript, pdf and PowerPoint
    if (!vector.graphic & (dev.type == "postscript" | 
                            dev.type == "pdf" | 
                            dev.type == "pptx") ) {
                           
      cat("\nConverting vector to raster graphics. Please wait.\n")
      tmp.file <- invisible(lapply(seq(n.plots), function (i) {
      
        raster.start.time  <- Sys.time()
        
        # Create tmp file
        tmp.file <- tempfile(fileext = ".png")
        # Open png device
        grDevices::png(tmp.file,
                       width = plot.width,
                       height = plot.height,
                       family = font.type,
                       pointsize = point.size,
                       res = ppi,
                       units = units)
        # Print plot
        print(plot.data[[i]])
        # close png device
        invisible(grDevices::dev.off())
        
        ETA(raster.start.time , i , n.plots)
        
        return (tmp.file)
        
      }))
      
    }
    
    # Save plots for other formats than PowerPoint
    if (dev.type != "pptx") {
      
      # Convert inches to pixles
      if (units == "px" & (dev.type != "pdf" | dev.type != "postscript")) {
        plot.width <- plot.width * ppi
        plot.height <- plot.height * ppi
      }
      
      # Convert inches to cm
      if (units == "cm" & (dev.type != "pdf" | dev.type != "postscript")) {
        plot.width <- plot.width * 2.54
        plot.height <- plot.height * 2.54
      }
      
      if (dev.type == "postscript" |  dev.type == "pdf" ) {
        
        if (graphic.type == "eps") grDevices::setEPS()
        dev.par <- sprintf("grDevices::%s(
                           file.name,
                           width = plot.width,
                           height = plot.height,
                           family = font.type,
                           pointsize = point.size,
                           onefile = one.file,
                           paper = 'special',
                           pagecentre = TRUE)", dev.type)
        
        # Other graphic decvices
      } else {
        dev.par <- sprintf("grDevices::%s(
                           file.name,
                           width = plot.width,
                           height = plot.height,
                           family = font.type,
                           pointsize = point.size,
                           res = ppi,
                           units = units)", dev.type)
      }
      
      # Evaluate and run graphics device drive
      eval(parse(text=dev.par))
      
      # If raster for ps/pdf
      cat(paste0("\nSaving plots as " , graphic.type , ". Please wait.\n"))
      if (!vector.graphic & (dev.type == "postscript" | 
                             dev.type == "pdf") ) {
        
        print.plot <- lapply(seq(n.plots), function (k) {
          plotPNG <- png::readPNG(tmp.file[[k]])
          graphics::par(mai=c(0,0,0,0))
          graphics::plot(c(0,1),c(0,1),type="n")
          graphics::rasterImage(plotPNG,0,0,1,1)
          
          ETA(plot.start.time , k , n.plots)
          
        })
        invisible(grDevices::dev.off())
        
      } else {
        
        print.plot <- lapply(seq(n.plots), function (k) {
          print(plot.data[[k]]) 
          
          ETA(plot.start.time , k , n.plots)
        })
        invisible(grDevices::dev.off())
        
      }
            
      # Save PowerPoint plots
      } else {
        
        cat(paste0("\nSaving plots as " , graphic.type , ". Please wait.\n"))
        lapply(1:length(file.name), function (i) {
        
         if (vector.graphic) {
           plot.data <- lapply(plot.data, function (x) {
              # Open new graphics device
              grDevices::dev.new(width=plot.width,
                                 height=plot.height,
                                 res=ppi,
                                 noRStudioGD = TRUE,
                                 units="in")
              # Print plots
              print(x)
              # Record graphics device
              p <- grDevices::recordPlot()
              grDevices::dev.off()
              return (p)
            } )
          }
          
          # Number of documents in PowerPoint file
          create.document <- if (one.file) seq(n.plots) else i
          
          # Define font type
          if (font.type == "serif") font.type <- "Times New Roman"
          
          # Select template
          template <- if (layout == "pt") "legacy" else if (layout == "apa") "apa" else "widescreen" 
          template.file <- paste0(system.file(package = 'bfw'),"/extdata/templates/",template,".pptx")
          
          # Create PowerPoint document
          document <- officer::read_pptx(template.file)
                    
          # Create slides
          lapply(create.document, function (j) {
            
            # Add new slide
            document <- officer::add_slide(document, "Title and Content", "Office Theme" )
            
            # If Vector graphics use rvg
            if (vector.graphic) { 
             
              # Create slide
              document <- officer::ph_with(document, 
                                       rvg::dml( code = print(plot.data[[j]]) ,
                                                 fonts = list(font.type),
                                                 pointsize = point.size ),
                                       location = officer::ph_location(
                                         left = (page.width - plot.width) / 2,
                                         top = (page.height - plot.height) / 2,
                                         width = plot.width,
                                         height = plot.height )
                                          
              )
              # Else use png device
            } else {
              
              # Add image to slide
              document <- officer::ph_with(x = document, 
                                           officer::external_img(tmp.file[[j]] ),
                                           location = officer::ph_location(
                                             left = (page.width - plot.width) / 2,
                                             top = (page.height - plot.height) / 2,
                                             width = plot.width,
                                             height = plot.height ) )
            }
            
            ETA(plot.start.time , j , n.plots)
            
          })
          
          # Write file
          invisible(print(document, target = file.name[[i]]))
          
        })
        
      }
  }
  
  if (save.data) {
    # Empty temp folder 
    if (exists("tmp.file")) unlink(paste0(tempdir(),"/*"),force=TRUE,recursive=TRUE)

    # If requested, return file names
    if (return.files) {
      if (grepl("%02d", file.name[[1]])) file.name <- sprintf(file.name,1:n.plots)
      return (file.name)
    }
  }
}