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
#' @param custom.model define a custom model to use (e.g., string or text file (.txt), Default: NULL
#' @param run.robust logical, indicating whether or not robust analysis, Default: FALSE
#' @param ... further arguments passed to or from other methods
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
                        custom.model,
                        run.robust,
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
  q.levels  <- apply(x.data, 2, function(x) length(unique(x[!is.na(x)]))) #Number of categories per x
  
  # name.contrasts for creating contrasts
  name.contrasts <- unique(lapply(DF[,x], function (x) as.list(levels(x))))
  
  # Number of x parameters
  n.x <- ncol(x.data)
  
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
  y.mean <- mean(y.data)
  y.sd <- stats::sd(y.data)
  y.gamma.prior <- unlist(GammaDist(sd(y.data)/2 , 2*sd(y.data)))
  x.row.sd <- stats::aggregate( y.data , unlist( apply(x.data, 2 , function(x) { 
    list(as.vector(x)) 
  }), recursive=FALSE) , FUN=sd )
  median.x.sd <- stats::median( x.row.sd$x , na.rm=TRUE )
  x.sd <- stats::sd( x.row.sd$x , na.rm=TRUE )
  x.gamma.prior <- unlist(GammaDist( median.x.sd , 2*x.sd))
  
  # Create data for Jags
  data.list <- list(
    y = y.data,
    x = x.data,
    n = n,
    q.levels = q.levels,
    y.mean = y.mean,
    y.sd = y.sd,
    y.gamma.prior = y.gamma.prior
  )
  
  # If robust add robust parameters
  if (run.robust) {
    data.list <- c(data.list,
                   median.x.sd = list(median.x.sd),
                   x.gamma.prior = list(x.gamma.prior))
  }
  
  # Paramter(s) of interest according to number of variables
  if(length(params)) {
    params <- TrimSplit(params)
  } else {
    params <- c(paste0("m",seq(n.x),collapse=""),
                paste0("s",seq(n.x),collapse="") )
  }
  
  # Create combinations of factors
  factors <- lapply(seq(n.x), function  (i) {
    as.matrix(t(combn(seq(n.x), i)))
  })
  
  
  # Create sigma
  sigma <- sprintf("1 / (%s[ %s ]) ^ 2" , 
                   paste0("s",seq(n.x),collapse="") , 
                   paste(sprintf("x[i,%s]" , seq(n.x)),collapse=",") )
                   
  sigma.prec <- paste(apply(tail(factors,1)[[1]], 1, function (x) {
    
    get.letters <- letters[(10+length(x))+seq(length(x))]
    letters <- paste( get.letters , collapse=",")
    
    start <- paste(lapply(1:length(x), function (i) {
      sprintf("for (%s in 1:q.levels[%s]) {\n", get.letters[i] , x[i])
    }),collapse="")
    
    end <- paste(lapply(rev(seq(x)), function (j) { 
      paste0( "\n" , "}" ) 
    }),collapse="")
    
    if (run.robust) {

      sigma <- sprintf("%s[%s] <- max(sigma.prec[%s], median.x.sd / 1000)\n" , 
                       paste0("s",seq(n.x),collapse="") , 
                       letters , 
                       letters )
      sigma.prec <- sprintf("sigma.prec[%s] ~ dgamma( sigma.r , sigma.l )" , 
                            letters )
    
    } else {
    
      sigma <- sprintf("%s[%s] ~ dgamma( 0.0001 , 0.0001 )" , 
                       paste0("s",seq(n.x),collapse=""),
                       letters )
      sigma.prec <- NULL
    
    }
    
    paste0(start,sigma,sigma.prec,end)
    
  }),collapse="\n\n")
  
  # Create factor list
  factors.additive <- paste(lapply(factors, function (x) {
    paste(apply(x, 1, function (y) {
      factor <- paste0("a",y,collapse="")
      matrix.pos <- paste0(sprintf("x[i,%s]",y),collapse=",")
      sprintf("%s[%s]",factor,matrix.pos)
    }),collapse="+\n")
  }),collapse="+\n")
  
  # Create effects
  effects <- paste(lapply(factors, function (x) {
    paste(apply(x, 1, function (x) {
      factor <- paste0("a",x,collapse="")
      factor.gamma <- sprintf("\n%s.sd ~ dgamma(y.gamma.prior[1], y.gamma.prior[2])", factor)
      
      start <- paste(lapply(1:length(x), function (i) {
        sprintf("for (%s in 1:q.levels[%s]) {\n", letters[10+i] , x[i])
      }),collapse="")
      
      dist <- sprintf("%s[%s] ~ dnorm(0.0, 1 / %s ^ 2)",
                      factor , 
                      paste(letters[seq(11,10+length(x))],collapse=",") , 
                      paste0(factor,".sd")
      )
      
      end <- paste(lapply(1:length(x), function (j) { 
        paste0( "\n" , "}" ) 
      }),collapse="")
      
      paste (start,dist,end,factor.gamma)
      
    }),collapse="\n\n")
  }),collapse="\n\n")
  
  # Create means matrix
  means <- paste(apply(tail(factors,1)[[1]], 1, function (x) {
    
    get.letters <- letters[(10+length(x))+seq(length(x))]
    
    start <- paste(lapply(1:length(x), function (i) {
      sprintf("for (%s in 1:q.levels[%s]) {\n", get.letters[i] , x[i])
    }),collapse="")
    
    end <- paste(lapply(rev(seq(x)), function (j) { 
      paste0( "\n" , "}" ) 
    }),collapse="")
    
    means <- paste(lapply(factors, function (x) {
      paste(apply(x, 1, function (y) {
        factor <- paste0("a",y,collapse="")
        matrix.pos <- paste0(get.letters[y],collapse=",")
        sprintf("%s[%s]",factor,matrix.pos)
      }),collapse=paste("+\n"))
    }),collapse=paste("+\n"))
    means <- sprintf("%s[%s] <- a0+\n%s",
                     paste0("m",x,collapse=""),
                     paste(get.letters,collapse=","),
                     means)
    
    paste(start,means,end)
    
  }),collapse="\n\n")
  
  # Replace placeholders in jags model with created values
  model.name <-  paste0(model.name,n.x)
  jags.model <- gsub("\\#\\bSIGMA\\b", sigma, jags.model)
  jags.model <- gsub("\\#SIGMAPREC", sigma.prec, jags.model)
  jags.model <- gsub("\\#FACTORS", factors.additive, jags.model)
  jags.model <- gsub("\\#EFFECTS", effects, jags.model)
  jags.model <- gsub("\\#MEANS", means, jags.model)
    
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