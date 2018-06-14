#' @title Predict Metric
#' @description Bayesian alternative to ANOVA
#' @param y criterion variable(s), Default: NULL
#' @param y.names optional names for criterion variable(s), Default: NULL
#' @param x categorical variable(s), Default: NULL
#' @param x.names optional names for categorical variable(s), Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param model.name name of model used
#' @param jags.model specify which module to use
#' @param ... further arguments passed to or from other methods
#' @return see metric vignette
#' @seealso
#'  \code{\link[stats]{complete.cases}},\code{\link[stats]{sd}},\code{\link[stats]{aggregate}},\code{\link[stats]{median}}
#' @rdname StatsMetric
#' @export
#' @importFrom stats complete.cases sd aggregate median
StatsMetric <- function(y,
                        y.names,
                        x,
                        x.names,
                        DF,
                        params,
                        job.group,
                        initial.list,
                        model.name,
                        jags.model,
                        ...
) {

  # Fetch y and x parameters
  x <- TrimSplit(x)
  y.names <- if (length(y.names)) TrimSplit(y.names) else CapWords(y)
  x.names <- if (length(x.names)) TrimSplit(x.names) else CapWords(x)

  # Create table for x.data
  DF <- DF[stats::complete.cases( DF[, c(y,x)] ), ]

  # Create X and Y metric count observations, number of parameters and categories in each parameter
  y.data <- DF[, y]
  x.data <- apply(as.matrix(DF[, x]), 2 , function (x) as.numeric(as.factor(x)))
  n <- length(y.data)
  q <- length(x.data[1,])
  q.levels  <- apply(x.data, 2, function(x) length(unique(x[!is.na(x)]))) #Number of categories per x

  # name.contrasts for creating contrasts
  name.contrasts <- unique(lapply(DF[,x], function (x) as.list(levels(x))))

  # Number of x parameters
  n.x <- length(name.contrasts)

  # Number of observations
  n <- length(y.data)

  # Create crosstable for x parameters
  n.data  <- as.data.frame(table(DF[, x]))

  # Create job names from contrast names
  single.names <- lapply(name.contrasts, function (x) unlist(x))

  # combine names from list 1 and 2
  combined.names <- lapply(1:length(single.names), function (i) {
    if (i<3) {
      a <- combn(single.names[[i]],2)
      apply(a,2,function (x) paste(x,collapse="/"))
    } else if (i>2) {
      single.names[[i]]
    }
  })

  # Reverse combinations from first naming list
  reversed.names <- lapply(1:length(single.names), function (i) {
    if (i<3) {
      a <- if (i==1) apply(combn(single.names[[1]],2),2,rev) else combn(single.names[[i]],2)
      apply(a,2,function (x) paste(x,collapse="/"))
    } else if (i>2) {
      single.names[[i]]
    }
  })

  # Final job names
  job.names <- list(single.names, combined.names, reversed.names)

  # Prior distributions
  yMean <- mean(y.data)
  ySD <- stats::sd(y.data)
  aGammaShRa <- unlist(GammaDist(sd(y.data)/2 , 2*sd(y.data)))
  cellSDs <- stats::aggregate( y.data , unlist( apply(x.data,2, function(x) list(as.vector(x))), recursive=FALSE) , FUN=sd )
  medianCellSD <- stats::median( cellSDs$x , na.rm=TRUE )
  sdCellSD <- stats::sd( cellSDs$x , na.rm=TRUE )
  sGammaShRa <- unlist(GammaDist( medianCellSD , 2*sdCellSD))

  # Create data for Jags
  data.list <- list(
    y = y.data,
    x = x.data,
    n = n,
    q.levels = q.levels,
    yMean = yMean,
    ySD = ySD,
    medianCellSD = medianCellSD,
    aGammaShRa = aGammaShRa,
    sGammaShRa = sGammaShRa
  )
  # Remove empty elements
  data.list <- data.list[lapply(data.list,length)>0]

  # Paramter(s) of interest according to number of variables
  if(length(params)) {
    params <- TrimSplit(params)
  } else {
    params <- c(paste0("m",seq(n.x),collapse=""),
                paste0("s",seq(n.x),collapse="") )
  }

  # Find relevant model according to number of x
  if (n.x>1) {
    if ( grepl("_robust", model.name) ) {
      model.name <- gsub("_robust",paste0(n.x, "_robust"),model.name)
    } else {
      model.name <-  paste0(model.name,n.x)
    }
    jags.model <- ReadFile( model.name , data.format = "txt" )
  }

  # Create name list
  name.list <- list(
    job.group = job.group,
    job.names = job.names,
    model.name = model.name
  )

  # Return data list
  return (list(
    data.list = data.list,
    name.list = name.list,
    params = params,
    n.data = n.data,
    jags.model = jags.model
  ))

}
