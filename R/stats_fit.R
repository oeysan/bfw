#' @title Fit Data
#' @description Apply latent or observed models to fit data (e.g., SEM, CFA, mediation)
#' @param latent laten variables, Default: NULL
#' @param latent.names optional names for for latent variables, Default: NULL
#' @param observed observed variable(s), Default: NULL
#' @param observed.names optional names for for observed variable(s), Default: NULL
#' @param additional supplemental parameters for fitted data (e.g., indirect pathways and total effect), Default: NULL
#' @param additional.names optional names for supplemental parameters, Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param model.name name of model used
#' @param jags.model specify which module to use
#' @param custom.model define a custom model to use (e.g., string or text file (.txt), Default: NULL
#' @param run.ppp logical, indicating whether or not to conduct ppp analysis, Default: FALSE
#' @param run.robust logical, indicating whether or not robust analysis, Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @seealso
#' \code{\link[stats]{complete.cases}}
#' @rdname StatsFit
#' @export
#' @importFrom stats complete.cases
StatsFit <- function(latent,
                     latent.names,
                     observed,
                     observed.names,
                     additional,
                     additional.names,
                     DF,
                     params,
                     job.group,
                     initial.list,
                     model.name,
                     jags.model,
                     custom.model,
                     run.ppp,
                     run.robust,
                     ...
) {
  
  # Select model type, observed or latent
  if (run.ppp & length(observed)) {
    model.type <- "cfa"
  } else if (length(observed)) {
    model.type <- "observed"
  } else if (length(latent)) {
    model.type <- "latent"
  } else {
    stop("Please select data to analyze.")
  }
  
  # Select data
  y <- if (length(latent)) TrimSplit(latent) else TrimSplit(observed)
  
  # Change name of jags model
  if(run.robust & !run.ppp) {
    model.name <- gsub("robust",paste0(model.type,"_robust"),model.name)
  } else {
    model.name <- paste0(model.name,"_",model.type)
  }
  
  # Create data matrix
  y <- as.matrix(DF[stats::complete.cases(DF[, y ]), y])
  
  # Name of observed variables
  name.stems <- unique ( gsub('[[:digit:]]+', '', colnames(y) ) )
  
  # Number of observations
  n <- nrow(y)
  
  # Number of latent variables
  q <- ncol(y)
  
  # Number of observed variables
  lat <- length(name.stems)
  
  # Select appropriate model
  if (!length(custom.model)) jags.model <- ReadFile( model.name , data.format = "txt" )
  
  if (length(observed)) {
    # Factor loading sequence
    factor.seq <- unlist ( lapply(1:lat, function (i){
      rep( i, length( grep( name.stems[[i]],colnames(y) ) ) )
    }) )
    
    # Fixed factor loadings sequence
    cl <- which(!duplicated(factor.seq))
    cl <- cbind(cl,c(cl-1,q)[-1])
    
    # Free factor loadings sequence
    fl <- which(duplicated(factor.seq))
  }  
  
  if (run.ppp) {
    # Priors Wishart Distribution
    psi.prec <- diag(1,lat)    
  } else {    
    # List of beta priors
    b.priors <- do.call(rbind,lapply(c(1:lat)[-1], function (x) {
      do.call(rbind,lapply(1:(x-1), function (y) {
        c(x,y)
      }))
    }))
    
    # Replace any static elements in JAGS model
    regression.model <-lapply(1:lat, function (j) {
      a <- sprintf("mu.eta[i , %s] <- alpha[%s]",j,j)
      if (j>1) {
        b <- paste (lapply(1:(j-1), function (k) {
          sprintf("+ beta[%s , %s] * eta[i, %s]", j,k,k)
        } ), collapse=" ")
        a <- paste(a,b)
      }
      return (a)
    } )
    regression.model <- paste(regression.model, collapse="\n")
    jags.model <- gsub("\\#REGRESSIONMODEL", regression.model, jags.model)
    
    
    # if additional parameters are present convert "human" input to machine input
    if (length(additional)) {
      # Add additional parameters
      additional <- RemoveEmpty(TrimSplit(gsub("\n",",",additional),","))
      additional <- lapply(additional, function (x) {
        x <- gsub("=","<-",x)
        label <- TrimSplit(x,"<-")[1]
        formula.org <- TrimSplit(x,"<-")[2]
        formula <- TrimSplit(formula.org,"\\*|\\/|\\+|\\-")
        formula <- gsub("[^[:alnum:]]","",formula)
        
        replace <- unlist(lapply(formula, function (y) {
          pos <- sort(unlist(lapply(name.stems, function (z) {
            if (regexpr(z,y)>0) {
              pos <- regexpr(z,y)[1]
              names(pos) <- which(name.stems == z)
              return (pos)
            }
          })),decreasing = TRUE)
          sprintf("beta[%s,%s]", names(pos)[1] , names(pos)[2] )
        }))
        formula <- VectorSub(formula , replace , x)
        if (!length(additional.names)) {
          names <- formula.org
        } else {
          names <- TrimSplit(additional.names)
        }
        
        return (list(label , formula , names))
      })
      additional <- lapply(1:length(additional[[1]]), function (i) sapply(additional, "[[", i))
      names(additional) <- c("params","formula","names")
      
      jags.additional <- paste(additional$formula, collapse="\n")
      jags.model <- gsub("\\#ADDITIONAL", jags.additional, jags.model)
    }
    
  }
  
  # Paramter(s) of interest
  if(length(params)) {
    params <- TrimSplit(params)
  } else {
    lambda <- if(length(observed)) c("lam", "error") else NULL
    params <- if (run.ppp) c( lambda , "cov") else c( lambda , "beta", "zbeta")
  }
  
  # Create data for Jags
  if (length(observed)) {
    data.list <- list(
      y = y,
      n = n,
      q = q,
      lat = lat,
      factor.seq = factor.seq,
      cl = cl,
      fl = fl)
    
    if (run.ppp) {
      data.list <- c(data.list, list(psi.prec = psi.prec))
    } else {
      data.list <- c(data.list, list(b.priors = b.priors)) 
    }
  } else {
    data.list <- list(
      n = n,
      lat = lat,
      eta = y,
      b.priors = b.priors)
  }
  
  # Define name group
  if (is.null(job.group)) job.group <- list ( c("lam","error") , c("cov","beta","zbeta") )
  
  # Add observed names if present
  if(length(observed.names)) {
    observed.names <- TrimSplit(observed.names)
  } else if(length(observed)) {
    observed.names <- CapWords(colnames(y))
  }
  
  # Add latent names if present
  if(length(latent.names)) {
    latent.names <- TrimSplit(latent.names)
  } else if(!length(latent)) {
    latent.names <- CapWords(name.stems)
  }
  
  # Create job names
  job.names <- list(
    list(observed.names),
    list(latent.names)
  )
  
  # Add additional parameters and job names if present
  if (length(additional)) {
    params <- c(params , additional$params)
    job.group <- c(job.group,as.list(additional$params))
    job.names <- c(job.names,as.list(additional$names))
  }
  
  # Create name list
  name.list <- list(
    job.group = job.group,
    job.names = job.names,
    model.name = model.name,
    model.type = model.type
  )
  
  # Return data list
  return (list(
    data.list = data.list,
    name.list = name.list,
    params = params,
    jags.model = jags.model,
    n.data = as.matrix(n)
  ))  
}