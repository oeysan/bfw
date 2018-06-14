#' @title Capitalize Words
#' @description capitalize the first letter in each words in a string
#' @param s string 
#' @param strict logical, indicating whether or not string it set to title case , Default: FALSE
#' @return returns capitalized string
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  CapWords("example eXAMPLE", FALSE)
#'  # [1] "Example EXAMPLE"
#'  CapWords("example eXAMPLE", TRUE)
#'  # [1] "Example Example"
#'  }
#' }
#' @rdname CapWords
#' @export 

CapWords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)), { 
    s <- substring(s, 2); if(strict) tolower(s) else s 
  }, sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#' @title Distinct Colors
#' @description create vector containing Hex color codes
#' @param range number of colors as sequence
#' @param random logical, indicating whether or not to provide random colors, Default: FALSE
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  DistinctColors(1:3)
#'  # [1] "#FFFF00" "#1CE6FF" "#FF34FF"
#'  set.seed(1)
#'  DistinctColors(1:3, TRUE)
#'  # [1] "#575329" "#CB7E98" "#D86A78"
#'  }
#' }
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
#' @export 

FlattenList <- function(li, rm.duplicated = TRUE, unname.li = TRUE, rm.empty = TRUE) {
  # process argument
  f <- function (l) if( class(l) == 'list') lapply(l, f) else enquote(l)
  # evaluate argument
  fi <- lapply(lapply(unlist(f(li)), eval),unlist)
  if (rm.duplicated) fi <- fi[!duplicated(fi)]
  if (unname.li) fi <- unname(fi)
  if (rm.empty) fi[lengths(fi) > 0L]
}

#' @title Gamma Distribution
#' @description compute gamma distribution (shape and rate) from mode and and standard deviation
#' @param mode mode from data
#' @param sd standard deviation from data
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  GammaDist(1,0.5)
#'  # $shape
#'  # [1] 5.828427
#'  # $rate
#'  # [1] 4.828427
#'  }
#' }
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
#' @param var variable of interest (e.g., Var)
#' @param range range of variables with same stem name (e.g., Var1, Var2, ..., Var8) , Default: 1:8
#' @param df data to extract from, Default: DF
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  data <- as.data.frame(matrix(1:80,ncol=8))
#'  colnames(data) <- paste0("Var", 1:8)
#'  GetRange("Var", 4:5, data)
#'  #   Var4 Var5
#'  #1    31   41
#'  #2    32   42
#'  #3    33   43
#'  #4    34   44
#'  #5    35   45
#'  #6    36   46
#'  #7    37   47
#'  #8    38   48
#'  #9    39   49
#'  #10   40   50
#'  }
#' }
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
#' \dontrun{
#' if(interactive()){
#'  a <- 1:3
#'  b <- LETTERS[1:3]
#'  Interleave(a,b)
#'  # [1] "1" "A" "2" "B" "3" "C"
#'  }
#' }
#' @rdname Interleave
#' @export 

Interleave <- function(a,b) {
  c(a,b)[ order( c( seq_along(a),
                    seq_along(b) ) ) ]
}

#' @title Is String
#' @description determine whether input is a single string 
#' @param x string
#' @return true or false
#' @rdname IsString
#' @export 

IsString <- function(x) {
  is.character(x) & length(x) == 1
}

#' @title Java Garbage
#' @description Call rJava function jcall to clear java cache
#' @seealso 
#'  \code{\link[rJava]{.jcall}}
#' @rdname JavaGarbage
#' @export 
#' @importFrom rJava .jcall

JavaGarbage <- function() {
  rJava::.jcall("java/lang/System", method = "gc")
  gc()
}

#' @title Normalize
#' @description simple function to normalize data
#' @param x numeric vector to normalize
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  Normalize(1:10)
#'  # [1] 0.01818182 0.03636364 0.05454545 0.07272727 0.09090909 
#'        0.10909091 0.12727273 0.14545455 0.16363636 0.18181818
#'  }
#' }
#' @rdname Normalize
#' @export 

Normalize <- function(x) { return( x / sum(x) ) }

#' @title Pad Vector
#' @description Pad a numeric vector according to the highest value
#' @param v numeric vector to pad
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  PadVector(1:10)
#'  # [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "10"
#'  }
#' }
#' @rdname PadVector
#' @export 

PadVector <- function(v) {
  gsub("\\s", "0", format(v, width=max(nchar(v))))
}

#' @title Paper Size
#' @description collection of print sizes
#' @param x select medium to print on, Default: 'a4'
#' @return width and height of select medium
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  PaperSize()
#'  # [1]  8.3 11.7
#'  }
#' }
#' @rdname PaperSize
#' @export 

PaperSize <- function(x = "a4") {
  
  x <- Trim(tolower(x))
  switch (x,
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
}

#' @title Parse Numbers
#' @description simple function to extract numbers from string/vector
#' @param x string or vector
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  ParseNumber("String1WithNumbers2")
#'  # [1] 1 2
#'  }
#' }
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
#' @param path path to file, Default: 'Models/'
#' @param package choose package to open from, Default: 'bfw'
#' @param type Type of file (i.e., text or data), Default: 'string'
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param data.format define what data format is being used, Default: 'csv'
#' @param custom logical, indicating whether or not to use custom file, , Default: FALSE
#' @seealso 
#'  \code{\link[utils]{read.csv}}
#' @rdname ReadFile
#' @export 
#' @importFrom utils read.csv

ReadFile <- function(file = NULL , path = "Models/" , package = "bfw" , type="string" , sep = "," , data.format = "csv" , custom = FALSE) {
  if (is.null(file)) stop("Please specify a file. Quitting.")
  if (!custom) file <- system.file(paste0("extdata/",path), paste0(file,".",data.format), package=package)
  if (file == "") {
      file <- NULL
  } else {
      if (RemoveSpaces(tolower(type)) == "string") file <- paste(readLines(file,warn=FALSE), collapse="\n")
      if (RemoveSpaces(tolower(type)) == "data") file <- utils::read.csv(file , head = TRUE , sep = sep)
  }
  return (file)
}

#' @title Remove Empty
#' @description Remove empty elements in vector
#' @param x vector to eliminate NA and blanks 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  RemoveEmpty( c("",NA,"","Remains") )
#'  # [1] "Remains"
#'  }
#' }
#' @rdname RemoveEmpty
#' @export 

RemoveEmpty <- function (x) {
  x <- Trim(x[!is.na(x)])
  x[!x == ""]
}

#' @title Remove Spaces
#' @description simple function to remove whitespace
#' @param x string
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  RemoveSpaces("  No More S p a c e s")
#'  # [1] "NoMoreSpaces"
#'  }
#' }
#' @rdname RemoveSpaces
#' @export 

RemoveSpaces <- function(x) gsub("[[:space:]]", "", x)

#' @title Save Name
#' @description simple function to construct a save name for data
#' @param project name of project, Default: 'Project'
#' @param dataset define subset of data, Default: 'AllData'
#' @param type type of data, Default: 'Model'
#' @param name save name, Default: 'Name'
#' @param unix logical, indicating whether or not to add unix timpestamp to save name, Default: TRUE
#' @param ... further arguments passed to or from other methods
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  SaveName()
#'  # [1] "ProjectAllData-Model-Name-1528408490"
#'  }
#' }
#' @rdname SaveName
#' @export 

SaveName <- function ( project = "Project" , dataset = "AllData" , type = "Model" ,  name = "Name" , unix = TRUE , ...) {
  sName <- paste0(project, dataset)
  sName <- paste(sName, type, sep="-")
  unix <- if (unix) paste0("-" , as.integer(Sys.time()) ) else NULL 
  sName <- RemoveSpaces(CapWords(paste0(sName, "-", name, unix)))
  
  return (sName)
}

#' @title Save Plot
#' @description Display and/or save plots
#' @param plot.data a list of plots
#' @param file save name, Default: 'Results/Generic'
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
#' @param paper define a print size for visualizations, Default: 'pw'
#' @param ... further arguments passed to or from other methods
#' @seealso 
#'  \code{\link[grDevices]{dev.new}},\code{\link[grDevices]{recordPlot}},\code{\link[grDevices]{setEPS}},\code{\link[grDevices]{graphics.off}},\code{\link[grDevices]{dev.list}},\code{\link[grDevices]{dev.off}}
#'  \code{\link[ReporteRs]{pptx}},\code{\link[ReporteRs]{addSlide}},\code{\link[ReporteRs]{addPlot}},\code{\link[ReporteRs]{writeDoc}}
#' @rdname SavePlot
#' @export 
#' @importFrom grDevices dev.new recordPlot setEPS graphics.off dev.list dev.off
#' @importFrom ReporteRs pptx addSlide addPlot writeDoc
SavePlot <- function ( plot.data,
                       file = "Results/Generic", 
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
                       paper = "pw",
                       ...) {
                       
  # Trim and lowercase graphic type
  graphic.type <- Trim(tolower(graphic.type))
  if (graphic.type == "ppt") graphic.type <- "pptx"
  if (graphic.type == "jpg") graphic.type <- "jpeg"
  
  # Set graphics device driver (if eps/ps set postscript else use filename extension)
  dev.type <- if (graphic.type == "eps" | graphic.type == "ps") "postscript" else graphic.type
  
  # Trim and split plot size
  plot.size <- as.numeric(TrimSplit(plot.size))
  
  # Create final save name 
  if (one.file & ( graphic.type == "ps" | 
                   graphic.type == "eps" | 
                   graphic.type == "pdf" | 
                   graphic.type == "pptx" ) ) {
    file <- paste0( paste( TrimSplit(file,"-")[1:3],collapse="-"),
                    "-Plot-", TrimSplit(file,"-")[4],
                    ".",
                    graphic.type)
  } else {
    file <- paste0( paste( TrimSplit(file,"-")[1:3],collapse="-"),
                    "-Plot%02d-", TrimSplit(file,"-")[4],
                    ".",
                    graphic.type)
  }
  
  if (!one.file & graphic.type == "pptx") {
    file <- unlist(lapply(PadVector(seq(length(plot.data))), function (i) gsub("%02d",i,file) ) )
  }
  
  # Use width / height if aspect ratio is not defined
  if (is.null(plot.aspect)) plot.aspect <- plot.size[[1]] / plot.size[[2]]
  # Extract width of page
  page.width <- PaperSize(paper)[1]
  # Extract height of page
  page.height <- PaperSize(paper)[2]
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
  
  if (dev.type == "pptx" | !save.data) {
    
    lapply(1:length(file), function (i) {
      
      # Open new graphics device
      grDevices::dev.new(width=plot.width, height=plot.height, res=ppi, units="in")
      # Print plots
      plot.data <- lapply(plot.data, function (x) {
        print(x)
        # Record graphics device
        p <- grDevices::recordPlot()
        dev.off()
        return (p)
      } )
      
      if (!save.data) {
        # Print plots
        print.plot <- lapply(plot.data, function (x) { 
          # Open new graphics device
          grDevices::dev.new(width=plot.width, height=plot.height, noRStudioGD = TRUE, res=ppi, units="in")
          print(x) 
        })
      }
      
      if (dev.type == "pptx" & save.data) {
        
        # Number of documents in PowerPOint file
        create.document <- if (one.file) seq(length(plot.data)) else i
        
        # Define font type
        if (font.type == "serif") font.type <- "Times New Roman"
        
        # Create PowerPoint document
        document <- ReporteRs::pptx()
        
        # Create slides
        lapply(create.document, function (j) {
          
          # Add new slide
          document <- ReporteRs::addSlide(document, slide.layout = "Blank")
          
          # Create slide
          document <- ReporteRs::addPlot(document,
                                         function() print(plot.data[[j]]),
                                         fontname_sans = font.type,
                                         vector.graphic = vector.graphic,
                                         pointsize = point.size,
                                         offx = (page.width - plot.width) / 2,
                                         offy = (page.height - plot.height) / 2,
                                         width = plot.width,
                                         height = plot.height)
        })
        
        # Write file
        ReporteRs::writeDoc(document, file = file[[i]])
        
      }
    })
    
  } else {
    
    
    # Convert inches to pixles
    if (units == "px" & (dev.type != "pdf" | dev.type != "postscript")) {
      plot.width <- plot.width * ppi
      plot.height <- plot.height * ppi
    }
    
    # Convert inches to pixles
    if (units == "cm" & (dev.type != "pdf" | dev.type != "postscript")) {
      plot.width <- plot.width * 2.54
      plot.height <- plot.height * 2.54
    }
    
    if (graphic.type == "ps" | graphic.type == "eps" | graphic.type == "pdf") {
      
      if (graphic.type == "eps") grDevices::setEPS()
      dev.par <- sprintf("grDevices::%s(
                         file,
                         width = plot.width,
                         height = plot.height,
                         family = font.type,
                         pointsize = point.size,
                         onefile = one.file,
                         paper = 'special',
                         pagecentre = TRUE)", dev.type)
    } else {
      dev.par <- sprintf("grDevices::%s(
                         file,
                         width = plot.width,
                         height = plot.height,
                         family = font.type,
                         pointsize = point.size,
                         res = ppi,
                         units = units)", dev.type)
      
      
    }
    
    # Evaluate and run graphics device drive 
    eval(parse(text=dev.par))
    # Print plots
    print.plot <- lapply(plot.data, function (x) print(x) )
  }
  
  # Close all graphics
  if (save.data) grDevices::graphics.off()
  # Turn off graphics device drive
  if (save.data & !is.null(grDevices::dev.list())) invisible(grDevices::dev.off())
  
  # Return save names if plots are saved
  if (save.data) {
    if (grepl("%02d", file[[1]])) file <- sprintf(file,1:length(plot.data))
    return (file)
  }
  
}

#' @title Trim
#' @description remove excess whitespace from string
#' @param s string
#' @param multi logical, indicating whether or not to remove excess whitespace between characters, Default: TRUE
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  Trim("             Trimmed      string")
#'  # [1] "Trimmed string"
#'  Trim("             Trimmed      string", FALSE)
#'  # [1] "Trimmed      string"
#'  }
#' }
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
#' @details \link[base]{strsplit}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  TrimSplit("Data 1,     Data2, Data3")
#'  # [1] "Data 1" "Data2"  "Data3"
#'  }
#' }
#' @rdname TrimSplit
#' @export 

TrimSplit <- function(x , 
                      sep = ",", 
                      fixed = FALSE, 
                      perl = FALSE, 
                      useBytes = FALSE) {
  
  x <- strsplit(as.character(x), sep , fixed, perl, useBytes)
  return(Trim(unlist(x)))
}

#' @title Pattern Matching and Replacement From Vectors
#' @description extending gsub by matching pattern and replacement from two vectors 
#' @param pattern vector containing words to match
#' @param replacement vector containing words to replace existing words.
#' @param string string to replace from
#' @return modified string with replaced values
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  pattern <- c("A","B","C")
#'  replacement <- 1:3
#'  string <- "A went to B went to C"
#'  VectorSub(pattern,replacement,string)
#'  # [1] "1 went to 2 went to 3"
#'  }
#' }
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