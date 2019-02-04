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
#' @param custom.model define a custom model to use (e.g., string or text file (.txt), Default: NULL
#' @param ... further arguments passed to or from other methods
#' @examples
#' # Use cats data
#' \donttest{
#' mcmc <- bfw(project.data = bfw::Cats,
#'             x = "Reward,Dance,Alignment",
#'             saved.steps = 50000,
#'             jags.model = "nominal",
#'             run.contrasts = TRUE,
#'             jags.seed = 100)
#' }
#' # Print only odds-ratio and effect sizes
#' \donttest{
#'    mcmc$summary.MCMC[ grep("Odds ratio|Effect",
#'                        rownames(mcmc$summary.MCMC)) , c(3:7) ]
#' }
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
#' @rdname StatsNominal
#' @export

StatsNominal <- function(x = NULL,
                         x.names = NULL,
                         DF,
                         params = NULL,
                         job.group = NULL,
                         initial.list = list(),
                         model.name,
                         jags.model,
                         custom.model = NULL,
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
  n.cell <- length(y.data) # Number of cells
  q.levels  <- apply(as.matrix(x.data[,1:n.x]), 2, function(x) length(unique(x[!is.na(x)]))) #Number of categories per x
  x.data <- as.matrix(expand.grid(lapply(q.levels, function (x) seq(x)))) # x as numeric
    if (n.x == 1) colnames(x.data) <- x.names # Add column name if only 1 x
  xC    <- lapply(1:n.x, function (i) t(combn(unique(x.data[,i]),2))) #combinations of categorices within each x
  
  # Prior distributions
  log.mean = log(sum(y.data) / n.cell )
  log.sd = log( sd(c(rep(0, n.cell - 1), sum(y.data))))
  gamma.prior = unlist(GammaDist(mode = log.sd, sd = 2 * log.sd))
  
  # Create data for Jags
  data.list <- list(
    y = y.data,
    x = x.data,
    n.cell = n.cell,
    q.levels = q.levels,
    log.mean = log.mean,
    log.sd = log.sd,
    gamma.prior = gamma.prior
  )

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
  
  # Create combinations of factors
  factors <- lapply(seq(n.x), function  (i) {
    as.matrix(t(combn(seq(n.x), i)))
  })
  
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
      factor.gamma <- sprintf("\n%s.sd ~ dgamma(gamma.prior[1], gamma.prior[2])", factor)
      
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
  means <- paste(apply(utils::tail(factors,1)[[1]], 1, function (x) {
    
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
  
  # Create predicted matrix
  predicted <- paste(apply(utils::tail(factors,1)[[1]], 1, function (x) {
    
    get.letters <- letters[(10+length(x))+seq(length(x))]
    letters <- paste(get.letters,collapse=",")
    parameter <- paste0("o",x,collapse="")
    
    start <- paste(lapply(1:length(x), function (i) {
      sprintf("for (%s in 1:q.levels[%s]) {\n", get.letters[i] , x[i])
    }),collapse="")
    
    end <- paste(lapply(rev(seq(x)), function (j) { 
      paste0( "\n" , "}" ) 
    }),collapse="")
    
    predicted <- sprintf("%s[%s] <- exp(%s[%s])", 
                         parameter,
                         letters,
                         paste0("m",x,collapse=""),
                         letters
    )
    
    predicted.prob <- sprintf("%sp[%s] <- ( %s[%s] / sum ( %s[%s] ) ) * 100", 
                              parameter,
                              letters,
                              parameter,
                              letters,
                              parameter,
                              paste(sprintf("1:q.levels[%s]", seq(length(x))),collapse=",\n")
                              
    )
    
    paste0(start , predicted , "\n" , predicted.prob , end)
    
  }),collapse="\n\n")
  
  # Create expected matrix
  expected <- paste(apply(utils::tail(factors,1)[[1]], 1, function (x) {
    
    get.letters <- letters[(10+length(x))+seq(length(x))]
    letters <- paste(get.letters,collapse=",")
    parameter <- paste0("o",x,collapse="")
    
    start <- paste(lapply(1:length(x), function (i) {
      sprintf("for (%s in 1:q.levels[%s]) {\n", get.letters[i] , x[i])
    }),collapse="")
    
    end <- paste(lapply(rev(seq(x)), function (j) { 
      paste0( "\n" , "}" ) 
    }),collapse="")
    
    
    if (length(get.letters)>1) { 
    
        if (length(get.letters)>2) { 
          conditional <- paste(" ," , paste(get.letters[3:length(get.letters)],collapse=" , "))
        } else { 
          conditional <- "" 
        }
        
        part.a <- sprintf("%s , 1:q.levels[2]%s" , get.letters[1] , conditional)
        part.b <- sprintf("1:q.levels[1] , %s%s" , get.letters[2] , conditional)
        part.c <- sprintf("1:q.levels[1] , 1:q.levels[2]%s" , conditional)
        
        expected <- sprintf("%s[%s] <- ( ( sum( %s[%s] ) *  \n sum( %s[%s] ) ) / \n sum( %s[%s] ) )", 
                            paste0("e",x,collapse=""),
                            letters,
                            parameter,
                            part.a,
                            parameter,
                            part.b,
                            parameter,
                            part.c
                            
        ) 
    } else {
        expected <- sprintf("%s[%s] <- sum( %s[ 1:q.levels[1] ] ) / q.levels[1]" , 
                            paste0("e",x,collapse=""),
                            letters,
                            parameter
        )
    }
    
    expected.prob <- sprintf("%sp[%s] <- ( %s[%s] / sum ( %s[%s] ) ) * 100", 
                             paste0("e",x,collapse=""),
                             letters,
                             paste0("e",x,collapse=""),
                             letters,
                             parameter,
                             paste(sprintf("1:q.levels[%s]", seq(length(x))),collapse=",\n")
                             
    )
    
    paste0(start , expected , "\n" , expected.prob , end)
    
  }),collapse="\n\n")
  
  # Replace placeholders in jags model with created values
  model.name <-  paste0(model.name,n.x)
  jags.model <- gsub("\\#FACTORS", factors.additive, jags.model)
  jags.model <- gsub("\\#EFFECTS", effects, jags.model)
  jags.model <- gsub("\\#MEANS", means, jags.model)
  jags.model <- gsub("\\#PREDICTED", predicted, jags.model)
  jags.model <- gsub("\\#EXPECTED", expected, jags.model)

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