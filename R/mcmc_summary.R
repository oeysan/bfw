#' @title Summarize MCMC
#' @description The function provide a summary of each parameter of interest (mean, median, mode, effective sample size (ESS), HDI and n)
#' @param par defined parameter
#' @param par.names parameter names
#' @param job.names names of all parameters in analysis, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @param ROPE define range for region of practical equivalence (e.g., c(-0.05 , 0.05), Default: NULL
#' @param n.data sample size for each parameter
#' @param ... further arguments passed to or from other methods
#' @seealso 
#'  \code{\link[coda]{effectiveSize}}
#' @rdname SumMCMC
#' @export 
#' @importFrom coda effectiveSize

SumMCMC <- function(par, 
                    par.names, 
                    job.names = NULL, 
                    job.group = NULL, 
                    credible.region = 0.95, 
                    ROPE = NULL,
                    n.data,
                    ...
) {
  
  Mean <- mean(par)
  Median <- median(par)
  Mode <- density(par)$x[which.max(density(par)$y)]
  ESS <- unname(round(coda::effectiveSize(par)))
  HDI <- ComputeHDI(par, credible.region = credible.region)
  
  # Parameter names
  par.names <- AddNames(par.names, job.names, job.group)
  
  if (nrow(n.data) == 1 | length(table(n.data)) == 1) {
    n <- n.data[ 1 , ncol(n.data)]
  } else {
    
n <- TrimSplit(gsub(".*:","",par.names),"vs.|@|/| - ")
    if (length(unique(n)) < length(n)) {
      n <- max(n.data[,"n"])
    } else {
      n <- apply(n.data[-ncol(n.data)], 2, function (x) x %in% n)
      drop.n <- which( apply(n, 2, function (x) any(x)) == FALSE)
      if (max((drop.n),0)>0) n <- n[,-(drop.n)]
      n <- which(apply(as.matrix(n),1,function (x) all(x) == TRUE))
      n <- sum(n.data[ n , ncol(n.data)])
    }
  }
  
  if ( !is.null(ROPE) ) {
    ROPElo <- ( 100 * sum( par < ROPE[1] ) / length( par ) )
    ROPEhi <- ( 100 * sum( par > ROPE[2] ) / length( par ) )
    ROPEin <- 100 - ( ROPElo + ROPEhi )
    ROPEtxt <- c("ROPEmin", "ROPEmax", "ROPElo", "ROPEhi", "ROPEin")
    sM <- matrix( c( Mean, Median, Mode, ESS, HDI[1], HDI[2], ROPE[1], ROPE[2], ROPElo, ROPEhi, ROPEin, n ), 1, 12 )
  } else { 
    ROPEtxt <- NULL
    sM <- matrix( c( Mean, Median, Mode, ESS, HDI[1], HDI[2], n ), 1, 7 )
  } 
  
  rownames(sM) <- par.names
  colnames(sM) <- c("Mean", "Median", "Mode", "ESS", "HDIlo", "HDIhi", ROPEtxt, "n")
  
  return (sM)
}
