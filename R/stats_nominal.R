#' @title Predict Nominal
#' @description Bayesian alternative to chi-square test
#' @param x categorical variable(s), Default: NULL
#' @param x.names optional names for categorical variable(s), Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param model.name name of model used
#' @param jags.model specify which module to use
#' @param ... further arguments passed to or from other methods
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Use cats data
#' mcmc <- bfw(project.data = bfw::Cats,
#'             x = "Reward,Dance,Alignment",
#'             saved.steps = 50000,
#'             jags.model = "nominal",
#'             run.contrasts = TRUE,
#'             jags.seed = 100)
#' 
#' # Print only odds-ratio and effect sizes
#' mcmc$summary.MCMC[ grep("Odds ratio|Effect",
#'                         rownames(mcmc$summary.MCMC)) , c(3:7 ]
#' #                                                    Mode   ESS    HDIlo     HDIhi    n
#' # Odds ratio: Food/Affection vs. No/Yes           0.14586 44452  0.11426   0.18982 2000
#' # Odds ratio: Affection/Food vs. No/Yes           6.49442 44215  5.10392   8.46668 2000
#' # Effect size: Food/Affection vs. No/Yes         -1.05346 44304 -1.18519  -0.90825 2000
#' # Effect size: Affection/Food vs. No/Yes          1.05346 44304  0.90825   1.18519 2000
#' # Odds ratio: Food/Affection vs. Evil/Good        0.77604 45245  0.62328   0.98904 2000
#' # Odds ratio: Affection/Food vs. Evil/Good        1.25432 45225  0.99311   1.57765 2000
#' # Effect size: Food/Affection vs. Evil/Good      -0.12844 45222 -0.25510  -0.00115 2000
#' # Effect size: Affection/Food vs. Evil/Good       0.12844 45222  0.00115   0.25510 2000
#' # Odds ratio: No/Yes vs. Evil/Good               13.12995 43500 10.58859  16.49207 2000
#' # Odds ratio: Yes/No vs. Evil/Good                0.07393 43739  0.05909   0.09221 2000
#' # Effect size: No/Yes vs. Evil/Good               1.43361 43603  1.30715   1.55020 2000
#' # Effect size: Yes/No vs. Evil/Good              -1.43361 43603 -1.55020  -1.30715 2000
#' # Odds ratio: Food/Affection vs. No/Yes @ Evil    0.00848 31117  0.00527   0.01336 1299
#' # Odds ratio: Affection/Food vs. No/Yes @ Evil  104.20109 30523 66.55346 169.12331 1299
#' # Odds ratio: Food/Affection vs. No/Yes @ Good    2.44193 35397  1.65204   3.63743  701
#' # Odds ratio: Affection/Food vs. No/Yes @ Good    0.36685 35417  0.25478   0.55982  701
#' # Effect size: Food/Affection vs. No/Yes @ Evil  -2.58578 30734 -2.85450  -2.35471 1299
#' # Effect size: Affection/Food vs. No/Yes @ Evil   2.58578 30734  2.35471   2.85450 1299
#' # Effect size: Food/Affection vs. No/Yes @ Good   0.51934 35316  0.30726   0.73443  701
#' # Effect size: Affection/Food vs. No/Yes @ Good  -0.51934 35316 -0.73443  -0.30726  701
#' #
#' # The results indicate that evil cats are 13.13 times more likely than good cats to decline dancing
#' # Furthermore, when offered affection, evil cats are 104.20 times more likely to decline dancing,
#' # relative to evil cats that are offered food.
#'  }
#' }
#' @rdname StatsNominal
#' @export 

StatsNominal <- function(x,
                     x.names,
                     DF,
                     params,
                     job.group,
                     initial.list,
                     model.name,
                     jags.model,
                     ...
) {
  
  # Fetch x parameters
  x <- TrimSplit(x)
  x.names <- if (length(x.names)) TrimSplit(x.names) else CapWords(x)
  
  # Create crosstable for x parameters
  x.data  <- as.data.frame(table(DF[, x]))
  names(x.data) <- c(x.names, "Freq") #add names
  
  # Set at n data
  n.data <- x.data
  
  # name.contrasts for creating contrasts
  name.contrasts <- unique(lapply(DF[,x], function (x) as.list(levels(x))))
  
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
  
  # Number of X parameters
  n.x <- length(name.contrasts)
  
  # Number of observations
  n <- sum(x.data[,ncol(x.data)])
  
  y.data <- x.data[, ncol(x.data)] # Frequencies
  Ncell <- length(y.data) # Number of cells
  q.levels  <- apply(as.matrix(x.data[,1:n.x]), 2, function(x) length(unique(x[!is.na(x)]))) #Number of categories per x
  x.data <- as.matrix(expand.grid(lapply(q.levels, function (x) seq(x)))) # x as numeric

  Ncell <- length(y.data) # Number of cells
  q.levels  <- apply(x.data, 2, function(x) length(unique(x[!is.na(x)]))) #Number of categories per x
  xC    <- lapply(1:n.x, function (i) t(combn(unique(x.data[,i]),2))) #combinations of categorices within each x
  
  # Prior distributions
  yLogMean = log(sum(y.data) / Ncell )
  yLogSD = log( sd(c(rep(0, Ncell - 1), sum(y.data))))
  aGammaShRa = unlist(GammaDist(mode = yLogSD, sd = 2 * yLogSD))
  
  # Create data for Jags
  data.list <- list(
    y = y.data,
    x = x.data,
    Ncell = Ncell,
    q.levels = q.levels,
    yLogMean = yLogMean,
    yLogSD = yLogSD,
    aGammaShRa = aGammaShRa
  )
  # Remove empty elements
  data.list <- data.list[lapply(data.list,length)>0]
  
  # Paramter(s) of interest according to number of variables 
  if(length(params)) {
    params <- TrimSplit(params)
  } else {
    params <- c(paste0("m",seq(n.x),collapse=""), 
                paste0("e",seq(n.x),collapse=""),
                paste0(paste0("e",seq(n.x),collapse=""),"p"), 
                paste0("o",seq(n.x),collapse=""),
                paste0(paste0("o",seq(n.x),collapse=""),"p")
                )
  }
  
  if (n.x>1) {
    # Find relevant model according to number of x
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
