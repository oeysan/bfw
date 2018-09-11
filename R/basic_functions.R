#' @title Capitalize Words
#' @description capitalize the first letter in each words in a string
#' @param s string
#' @param strict logical, indicating whether or not string it set to title case , Default: FALSE
#' @return returns capitalized string
#' @examples
#'  CapWords("example eXAMPLE", FALSE)
#'  # [1] "Example EXAMPLE"
#'  CapWords("example eXAMPLE", TRUE)
#'  # [1] "Example Example"
#' @rdname CapWords
#' @export

CapWords <- function(s, strict = FALSE) {
  if (length(s)) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
      s <- substring(s, 2); if(strict) tolower(s) else s
    }, sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
}

#' @title Distinct Colors
#' @description create vector containing Hex color codes
#' @param range number of colors as sequence
#' @param random logical, indicating whether or not to provide random colors, Default: FALSE
#' @examples
#'  DistinctColors(1:3)
#'  # [1] "#FFFF00" "#1CE6FF" "#FF34FF"
#'  set.seed(1)
#'  DistinctColors(1:3, TRUE)
#'  # [1] "#575329" "#CB7E98" "#D86A78"
#' @rdname DistinctColors
#' @export
DistinctColors <- function(range, random = FALSE) {
  colors <- c("#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941",
              "#006FA6", "#A30059", "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC",
              "#B79762", "#004D43", "#8FB0FF", "#997D87", "#5A0007", "#809693",
              "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
              "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9",
              "#B903AA", "#D16100", "#DDEFFF", "#000035", "#7B4F4B", "#A1C299",
              "#300018", "#0AA6D8", "#013349", "#00846F", "#372101", "#FFB500",
              "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
              "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68",
              "#7A87A1", "#788D66", "#885578", "#FAD09F", "#FF8A9A", "#D157A0",
              "#BEC459", "#456648", "#0086ED", "#886F4C", "#34362D", "#B4A8BD",
              "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
              "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757",
              "#C8A1A1", "#1E6E00", "#7900D7", "#A77500", "#6367A9", "#A05837",
              "#6B002C", "#772600", "#D790FF", "#9B9700", "#549E79", "#FFF69F",
              "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
              "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804",
              "#324E72", "#6A3A4C", "#83AB58", "#001C1E", "#D1F7CE", "#004B28",
              "#C8D0F6", "#A3A489", "#806C66", "#222800", "#BF5650", "#E83000",
              "#66796D", "#DA007C", "#FF1A59", "#8ADBB4", "#1E0200", "#5B4E51",
              "#C895C5", "#320033", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94",
              "#7ED379", "#012C58", "#7A7BFF", "#D68E01", "#353339", "#78AFA1",
              "#FEB2C6", "#75797C", "#837393", "#943A4D", "#B5F4FF", "#D2DCD5",
              "#9556BD", "#6A714A", "#001325", "#02525F", "#0AA3F7", "#E98176",
              "#DBD5DD", "#5EBCD1", "#3D4F44", "#7E6405", "#02684E", "#962B75",
              "#8D8546", "#9695C5", "#E773CE", "#D86A78", "#3E89BE", "#CA834E",
              "#518A87", "#5B113C", "#55813B", "#E704C4", "#00005F", "#A97399",
              "#4B8160", "#59738A", "#FF5DA7", "#F7C9BF", "#643127", "#513A01",
              "#6B94AA", "#51A058", "#A45B02", "#1D1702", "#E20027", "#E7AB63",
              "#4C6001", "#9C6966", "#64547B", "#97979E", "#006A66", "#391406",
              "#F4D749", "#0045D2", "#006C31", "#DDB6D0", "#7C6571", "#9FB2A4",
              "#00D891", "#15A08A", "#BC65E9", "#FFFFFE", "#C6DC99", "#203B3C",
              "#671190", "#6B3A64", "#F5E1FF", "#FFA0F2", "#CCAA35", "#374527",
              "#8BB400", "#797868", "#C6005A", "#3B000A", "#C86240", "#29607C",
              "#402334", "#7D5A44", "#CCB87C", "#B88183", "#AA5199", "#B5D6C3",
              "#A38469", "#9F94F0", "#A74571", "#B894A6", "#71BB8C", "#00B433",
              "#789EC9", "#6D80BA", "#953F00", "#5EFF03", "#E4FFFC", "#1BE177",
              "#BCB1E5", "#76912F", "#003109", "#0060CD", "#D20096", "#895563",
              "#29201D", "#5B3213", "#A76F42", "#89412E", "#1A3A2A", "#494B5A",
              "#A88C85", "#F4ABAA", "#A3F3AB", "#00C6C8", "#EA8B66", "#958A9F",
              "#BDC9D2", "#9FA064", "#BE4700", "#658188", "#83A485", "#453C23",
              "#47675D", "#3A3F00", "#061203", "#DFFB71", "#868E7E", "#98D058",
              "#6C8F7D", "#D7BFC2", "#3C3E6E", "#D83D66", "#2F5D9B", "#6C5E46",
              "#D25B88", "#5B656C", "#00B57F", "#545C46", "#866097", "#365D25",
              "#252F99", "#00CCFF", "#674E60", "#FC009C", "#92896B")
  
  val <- if (random & length(range) > length(colors)) {
    sample(colors, length(range), replace = TRUE)
  } else if (random) {
    sample(colors, length(range))
  } else {
    colors[range]
  }
  val <- val[!is.na(val)]
  
  return (val)
}

#' @title Flatten List
#' @description flatten a nested list into a single list
#' @param li list to flatten
#' @param rm.duplicated logical, indicating whether or not to remove duplicated lists, Default: TRUE
#' @param unname.li logical, indicating whether or not to unname lists, Default: TRUE
#' @param rm.empty logical, indicating whether or not to remove empty lists, Default: TRUE
#' @rdname FlattenList
#' @examples
#' li <- list(LETTERS[1:3],
#'            list(letters[1:3],
#'                 list(LETTERS[4:6])),
#'            DEF = letters[4:6],
#'            LETTERS[1:3],
#'            list() # Emtpy list
#' )
#' print(li)
#' # [[1]]
#' # [1] "A" "B" "C"
#' #
#' # [[2]]
#' # [[2]][[1]]
#' # [1] "a" "b" "c"
#' #
#' # [[2]][[2]]
#' # [[2]][[2]][[1]]
#' # [1] "D" "E" "F"
#' #
#' #
#' #
#' # $DEF
#' # [1] "d" "e" "f"
#' #
#' # [[4]]
#' # [1] "A" "B" "C"
#' #
#' # [[5]]
#' # list()
#' FlattenList(li)
#' # [[1]]
#' # [1] "A" "B" "C"
#' #
#' # [[2]]
#' # [1] "a" "b" "c"
#' #
#' # [[3]]
#' # [1] "D" "E" "F"
#' #
#' # [[4]]
#' # [1] "d" "e" "f"
#' @export

FlattenList <- function(li, rm.duplicated = TRUE, unname.li = TRUE, rm.empty = TRUE) {
  # process argument
  f <- function (l) if( class(l) == 'list') sapply(l, f) else enquote(l)
  # evaluate argument
  fi <- lapply(lapply(unlist(f(li)), eval),unlist)
  if (rm.duplicated) fi <- fi[!duplicated(fi)]
  if (unname.li) fi <- unname(fi)
  if (rm.empty) fi[lengths(fi) > 0L]
}

#' @title Gamma Distribution
#' @description compute gamma distribution (shape and rate) from mode and standard deviation
#' @param mode mode from data
#' @param sd standard deviation from data
#' @examples
#'  GammaDist(1,0.5)
#'  # $shape
#'  # [1] 5.828427
#'  # $rate
#'  # [1] 4.828427
#' @rdname GammaDist
#' @export

GammaDist <- function(mode, sd) {
  if (mode <= 0) stop("mode must be > 0")
  if (sd <= 0) stop("sd must be > 0")
  rate = (mode + sqrt(mode ^ 2 + 4 * sd ^ 2)) / (2 * sd ^ 2)
  shape = 1 + mode * rate
  return (list(shape = shape, rate = rate))
}

#' @title Get Range
#' @description simple function to extract columns from data frame
#' @param var variable of interest (e.g., V)
#' @param range range of variables with same stem name (e.g., V1, V2, ..., V8) , Default: 1:8
#' @param df data to extract from
#' @examples
#' data <- as.data.frame(matrix(1:80,ncol=8))
#' GetRange("V", c(1,4), data)
#' #    V1 V4
#' # 1   1 31
#' # 2   2 32
#' # 3   3 33
#' # 4   4 34
#' # 5   5 35
#' # 6   6 36
#' # 7   7 37
#' # 8   8 38
#' # 9   9 39
#' # 10 10 40
#' @rdname GetRange
#' @export

GetRange <- function(var, range = 1:8, df) {
  return (df[paste0(var, range)])
}

#' @title Interleave
#' @description mix vectors by alternating between them
#' @param a first vector
#' @param b second vector
#' @return mixed vector
#' @examples
#'  a <- 1:3
#'  b <- LETTERS[1:3]
#'  Interleave(a,b)
#'  # [1] "1" "A" "2" "B" "3" "C"
#' @rdname Interleave
#' @export

Interleave <- function(a,b) {
  c(a,b)[ order( c( seq_along(a),
                    seq_along(b) ) ) ]
}

#' @title Single String
#' @description determine whether input is a single string
#' @param x string
#' @return true or false
#' @rdname SingleString
#' @examples
#' A <- "This is a single string"
#' SingleString(A)
#' # [1] TRUE
#' is.character(A)
#' # [1] TRUE
#' B <- c("This is a vector" , "containing two strings")
#' SingleString(B)
#' # [1] FALSE
#' is.character(B)
#' # [1] TRUE
#' @export

SingleString <- function(x) {
  is.character(x) & length(x) == 1
}

#' @title Normalize
#' @description simple function to normalize data
#' @param x numeric vector to normalize
#' @examples
#' Normalize(1:10)
#' # [1] 0.0182 0.0364 0.0545 0.0727 0.0909
#' # 0.1091 0.1273 0.1455 0.1636 0.1818
#' @rdname Normalize
#' @export

Normalize <- function(x) {
  return( x / sum(x) )
}

#' @title Pad Vector
#' @description Pad a numeric vector according to the highest value
#' @param v numeric vector to pad
#' @examples
#'  PadVector(1:10)
#'  # [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10"
#' @rdname PadVector
#' @export

PadVector <- function(v) {
  gsub("\\s", "0", format(v, width=max(nchar(v))))
}

#' @title Layout 
#' @description collection of layout sizes
#' @param x type of layout, Default: 'a4'
#' @param layout.inverse logical, indicating whether or not to inverse layout (e.g., landscape) , Default: FALSE
#' @return width and height of select medium
#' @examples
#'  Layout()
#'  # [1]  8.3 11.7
#' @rdname Layout
#' @export

Layout <- function(x = "a4", layout.inverse = FALSE) {
  
  x <- Trim(tolower(x))
  x <- switch (x,
               "pt" = c(10,7.5),
               "pw" = c(13.33,7.5),
               "4a0" = c(66.2,93.6),
               "2a0" = c(46.8,66.2),
               "a0" = c(33.1,46.8),
               "a1" = c(23.4,33.1),
               "a2" = c(16.5,23.4),
               "a3" = c(11.7,16.5),
               "a5" = c(5.8,8.3),
               "a6" = c(4.1,5.8),
               "a7" = c(2.9,4.1),
               "a8" = c(2,2.9),
               "a9" = c(1.5,2),
               "a10" = c(1,1.5),
               "a4" = c(8.3,11.7)
  )
  
  if (layout.inverse) x <- rev(x)
  
  return (x)
  
}

#' @title Parse Numbers
#' @description simple function to extract numbers from string/vector
#' @param x string or vector
#' @examples
#'  ParseNumber("String1WithNumbers2")
#'  # [1] 1 2
#' @seealso
#'  \code{\link[stats]{na.omit}}
#' @rdname ParseNumber
#' @export
#' @importFrom stats na.omit

ParseNumber <- function (x) {
  x <- c(stats::na.omit(as.numeric(TrimSplit(x,"\\D+"))))
  if (!length(x)) x <- 0
  return (x)
}

#' @title Read File
#' @description opens connection to a file
#' @param file name of file, Default: NULL
#' @param path path to file, Default: 'models/'
#' @param package choose package to open from, Default: 'bfw'
#' @param type Type of file (i.e., text or data), Default: 'string'
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param data.format define what data format is being used, Default: 'csv'
#' @param custom logical, indicating whether or not to use custom file, , Default: FALSE
#' @examples
#' # Print JAGS model for bernoulli trials
#' cat(ReadFile("stats_bernoulli"))
#' # model {
#' #   for (i in 1:n){
#' #     x[i] ~ dbern(theta)
#' #   }
#' #   theta ~ dunif(0,1)
#' # }
#' @seealso
#'  \code{\link[utils]{read.csv}}
#' @rdname ReadFile
#' @export
#' @importFrom utils read.csv

ReadFile <- function(file = NULL ,
                     path = "models/" ,
                     package = "bfw" ,
                     type="string" ,
                     sep = "," ,
                     data.format = "txt" ,
                     custom = FALSE) {
  
  type <- RemoveSpaces(tolower(type))
  
  if (is.null(file)) stop("Please specify a file. Quitting.")
  if (!custom) file <- system.file(paste0("extdata/",path),
                                   paste0(file,".",data.format),
                                   package=package)
  if (file == "") {
    file <- NULL
  } else {
    if (type == "string") file <- paste(readLines(file,warn=FALSE),
                                        collapse="\n")
    if (type == "data") file <- utils::read.csv(file ,
                                                head = TRUE ,
                                                sep = sep)
  }
  return (file)
}

#' @title Remove Empty
#' @description Remove empty elements in vector
#' @param x vector to eliminate NA and blanks
#' @examples
#'  RemoveEmpty( c("",NA,"","Remains") )
#'  # [1] "Remains"
#' @rdname RemoveEmpty
#' @export

RemoveEmpty <- function (x) {
  x <- Trim(x[!is.na(x)])
  x <- x[!x == ""]
}

#' @title Remove Spaces
#' @description simple function to remove whitespace
#' @param x string
#' @examples
#'  RemoveSpaces("  No More S p a c e s")
#'  # [1] "NoMoreSpaces"
#' @rdname RemoveSpaces
#' @export

RemoveSpaces <- function(x) gsub("[[:space:]]", "", x)

#' @title File Name
#' @description simple function to construct a file name for data
#' @param project name of project, Default: 'Project'
#' @param subset define subset of data, Default: NULL
#' @param type type of data, Default: NULL
#' @param name save name, Default: NULL
#' @param unix logical, indicating whether or not to append unix timestamp, Default: TRUE
#' @param ... further arguments passed to or from other methods
#' @examples
#'  FileName()
#'  # [1] "Project-Name-1528834963"
#'
#'  FileName(project = "Project" ,
#'          subset = "subset" ,
#'          type = "longitudinal" ,
#'          name = "cheese",
#'          unix = FALSE)
#'  # [1] "Projectsubset-longitudinal-cheese"
#' @rdname FileName
#' @export

FileName <- function ( project = "Project" ,
                       subset = NULL ,
                       type = NULL ,
                       name = "Name",
                       unix = TRUE ,
                       ...) {
  sName <- paste0(project, subset)
  if(length(type)) sName <- paste(sName, type, sep="-")
  unix <- if (unix) paste0("-" , as.integer(Sys.time()) )
  sName <- RemoveSpaces(CapWords(paste0(sName, "-", name, unix)))
  
  return (sName)
}

#' @title Parse Plot
#' @description Display and/or save plots
#' @param plot.data a list of plots
#' @param project.dir define where to save data, Default: 'Results/'
#' @param project.name define name of project, Default: 'FileName(name="Plot")'
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
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
#' project.name <- FileName(name="Testing")
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
#' project.name <- FileName(name="Testing")
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
#' \code{\link[grDevices]{dev.new}},
#' \code{\link[grDevices]{recordPlot}},
#' \code{\link[grDevices]{setEPS}},
#' \code{\link[grDevices]{graphics.off}},
#' \code{\link[grDevices]{dev.list}},
#' \code{\link[grDevices]{dev.off}}
#' \code{\link[graphics]{par}}
#' \code{\link[graphics]{plot}}
#' @rdname ParsePlot
#' @export
#' @importFrom grDevices dev.new recordPlot setEPS graphics.off dev.list dev.off
#' @importFrom graphics par plot

ParsePlot <- function (plot.data,
                       project.dir = "Results/",
                       project.name = FileName(name="Plot"),
                       graphic.type = "pptx",
                       plot.size = "15,10",
                       scaling = 100,
                       plot.aspect = NULL,
                       save.data = FALSE,
                       vector.graphic = TRUE,
                       point.size = 15,
                       font.type = "serif",
                       one.file = TRUE,
                       ppi = 300,
                       units = "in",
                       layout = "pw",
                       layout.inverse = FALSE,
                       return.files = FALSE,
                       ...) {
  
  # If single plot add plot to list
  if (typeof(plot.data[[1]]) != "list") plot.data <- list(plot.data)
  # Check if plot
  if (length((plot.data[[1]])) != 3) stop("This is not a plot.")
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
    if ( tail(TrimSplit(project.dir,""),1) != "/") {
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
      plot.type <- "-Plot"
    } else {
      plot.type <- "-Plot%02d"
    }
    
    # split file name
    names.vector <- TrimSplit(project.name,"-")
    # Number of elements in file name
    n.names <- length(names.vector)
    # Check if file name has unix time stamp
    time.stamp  <- nchar(ParseNumber(tail(names.vector,1))) > 9
    
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
      tmp.file <- lapply(seq(n.plots), function (i) {
      
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
        
      })
      
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
          template <- if (layout == "pt") "legacy" else "widescreen" 
          template.file <- paste0(system.file(package = 'bfw'),"/extdata/templates/",template,".pptx")
          
          # Create PowerPoint document
          document <- officer::read_pptx(template.file)
                    
          # Create slides
          lapply(create.document, function (j) {
            
            # Add new slide
            document <- officer::add_slide(document, "Blank", "Office Theme")
            
            # If Vector graphics use rvg
            if (vector.graphic) { 
             
              # Create slide
              document <- rvg::ph_with_vg(document, 
                                          print(plot.data[[j]]),
                                          fonts = list(font.type),
                                          type = NULL,
                                          pointsize = point.size,
                                          offx = (page.width - plot.width) / 2,
                                          offy = (page.height - plot.height) / 2,
                                          width = plot.width,
                                          height = plot.height
                                          
              )
              # Else use png device
            } else {
              
              # Add image to slide
              document <- officer::ph_with_img_at(x = document, 
                                                  src = tmp.file[[j]],
                                                  width = plot.width,
                                                  height = plot.height,
                                                  left = (page.width - plot.width) / 2,
                                                  top = (page.height - plot.height) / 2 )
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

#' @title Trim
#' @description remove excess whitespace from string
#' @param s string
#' @param multi logical, indicating whether or not to remove excess whitespace between characters, Default: TRUE
#' @examples
#'  Trim("             Trimmed      string")
#'  # [1] "Trimmed string"
#'  Trim("             Trimmed      string", FALSE)
#'  # [1] "Trimmed      string"
#' @rdname Trim
#' @export

Trim <- function(s, multi = TRUE) {
  if (multi) {
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", s, perl=TRUE)
  } else {
    gsub("^\\s+|\\s+$", "", s)
  }
}

#' @title Trim Split
#' @description Extends strsplit by trimming and unlisting string
#' @param x string
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param fixed logical, if TRUE match split exactly, otherwise use regular expressions. Has priority over perl, Default: FALSE
#' @param perl logical, indicating whether or not to use Perl-compatible regexps, Default: FALSE
#' @param useBytes logical. If TRUE the matching is done byte-by-byte rather than character-by-character, Default: FALSE
#' @param rm.empty logical. indicating whether or not to remove empty elements, Default: TRUE
#' @details \link[base]{strsplit}
#' @examples
#'  TrimSplit("Data 1,     Data2, Data3")
#'  # [1] "Data 1" "Data2"  "Data3"
#' @rdname TrimSplit
#' @export

TrimSplit <- function(x,
                      sep = ",",
                      fixed = FALSE,
                      perl = FALSE,
                      useBytes = FALSE,
                      rm.empty = TRUE) {
  
  # Split string by seperator
  x <- strsplit(as.character(x), sep , fixed, perl, useBytes)
  # Unlist and trim vector elements
  x <- Trim(unlist(x))
  # If selected remove empty elements
  if (rm.empty) x <- RemoveEmpty(x)
  if (identical(x, character(0))) x <- NULL
  
  return (x)
  
}

#' @title Pattern Matching and Replacement From Vectors
#' @description extending gsub by matching pattern and replacement from two vectors
#' @param pattern vector containing words to match
#' @param replacement vector containing words to replace existing words.
#' @param string string to replace from
#' @return modified string with replaced values
#' @examples
#'  pattern <- c("A","B","C")
#'  replacement <- 1:3
#'  string <- "A went to B went to C"
#'  VectorSub(pattern,replacement,string)
#'  # [1] "1 went to 2 went to 3"
#' @rdname VectorSub
#' @export

VectorSub <- function ( pattern , replacement , string ) {
  modified.string <- string
  lapply(1:length(pattern) , function (i) {
    modified.string <<- gsub(pattern[i] ,
                             replacement[i] ,
                             modified.string)
  })
  if (is.na(modified.string)) NULL else modified.string
}

#' @title Tidy Code
#' @description Small function that clears up messy code
#' @param tidy.code Messy code that needs cleaning
#' @param jags logical. If TRUE run code as JAGS model, Default: TRUE
#' @return (Somewhat) tidy code
#' @examples
#' messy <- "code <- function( x ) {
#' print (x ) }"
#' cat(messy)
#' code <- function( x ) {
#' print (x ) }
#' cat ( TidyCode(messy, jags = FALSE) )
#' code <- function(x) {
#'    print(x)
#' }
#' @rdname TidyCode
#' @export

TidyCode <- function(tidy.code,
                     jags = TRUE) {
  

  # if the code is a jags model replace model with placeholder
  if (jags) {
    tidy.code <- gsub("data[[:space:]]+\\{", "if (TidyJagsData) {" , tidy.code)
    tidy.code <- gsub("model[[:space:]]+\\{", "if (TidyJagsModel) {" , tidy.code)
  }
  
  # Extract blocks from cod
  tidy.code <- TrimSplit(tidy.code,"\\\n")
  
  # Wrap comments prior to parsing
  invisible(lapply(grep("\\#",tidy.code), function (i){
    tidy.code[i] <<- sprintf("invisible(\"StartPreParse%sEndPreParse\")" , tidy.code[i])
  }))
  
  # Parse code
  tidy.code <- base::parse(text = tidy.code, keep.source = FALSE)
  
  # Collapse parsed function into a vector
  tidy.code <- sapply(tidy.code, function(e) { 
    paste(base::deparse(e, getOption("width")), collapse = "\n")
  })
  
  # remove spaces between commas
  tidy.code <- gsub("\\s*\\,\\s*", "," , tidy.code)
  
  # Revert comments (remove invisibility)
  tidy.code <- gsub("invisible\\(\\\"StartPreParse" , "" , tidy.code)
  tidy.code <- gsub("EndPreParse\\\")" , "" , tidy.code)
  
  # If jags replace placeholder
  if (jags) {
    tidy.code <- gsub("if \\(TidyJagsData\\)", "data" , tidy.code)
    tidy.code <- gsub("if \\(TidyJagsModel\\)", "model" , tidy.code)
  }
  
  return (tidy.code)
}

#' @title ETA
#' @description Print estimated time for arrival (ETA)
#' @param start.time Start time (preset variable with Sys.time())
#' @param i incremental steps towards total
#' @param total Total number of steps
#' @seealso
#' \code{\link[utils]{flush.console}}
#' @rdname ETA
#' @export
#' @importFrom utils flush.console

ETA <- function (start.time, i , total) {
  eta <- Sys.time() + ( (total - i) * ((Sys.time() - start.time) / i) )
  eta.message <- sprintf("Progress: %.02f%% (%s/%s). ETA: %s ", 
                         (i * 100) / total,
                         i,
                         total,
                         format(eta,"%d.%m.%Y - %H:%M:%S"))
  cat("\r" , eta.message , sep="")
  utils::flush.console()
  if (i == total) cat("\n")
}