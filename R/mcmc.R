#' @title Run MCMC
#' @description Conduct MCMC simulations using JAGS
#' @param jags.model specify which module to use
#' @param params define parameters to observe, Default: NULL
#' @param name.list list of names
#' @param data.list list of data
#' @param initial.list initial values for analysis, Default: list()
#' @param run.contrasts logical, indicating whether or not to run contrasts, Default: FALSE
#' @param use.contrast choose from "between", "within" and "mixed". Between compare groups at different conditions. Within compare a group at different conditions. Mixed compute all comparisons, Default: "between",
#' @param contrasts define contrasts to use for analysis (defaults to all) , Default: NULL
#' @param run.ppp logical, indicating whether or not to conduct ppp analysis, Default: FALSE
#' @param k.ppp run ppp for every kth length of MCMC chains, Default: 10
#' @param n.data sample size for each parameter
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
#' @param ROPE define range for region of practical equivalence (e.g., c(-0.05 , 0.05), Default: NULL
#' @param merge.MCMC logical, indicating whether or not to merge MCMC chains, Default: FALSE
#' @param run.diag logical, indicating whether or not to run diagnostics, Default: FALSE
#' @param param.diag define parameters to use for diagnostics, default equals all parameters, Default: NULL
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#495054", "#e3e8ea")
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
#' @param ... further arguments passed to or from other methods
#' @return list containing MCMC chains , MCMC chains as matrix , summary of MCMC, list of name used, list of data, the jags model, running time of analysis and names of saved files
#' @seealso 
#'  \code{\link[runjags]{runjags.options}},\code{\link[runjags]{run.jags}}
#'  \code{\link[parallel]{detectCores}}
#'  \code{\link[coda]{as.mcmc.list}},\code{\link[coda]{varnames}}
#'  \code{\link[plyr]{rbind.fill}}
#'  \code{\link[stats]{cor}},\code{\link[stats]{cov}},\code{\link[stats]{sd}}
#'  \code{\link[MASS]{mvrnorm}}
#'  \code{\link[utils]{write.table}}
#' @rdname RunMCMC
#' @export 
#' @importFrom runjags runjags.options run.jags
#' @importFrom parallel detectCores
#' @importFrom coda as.mcmc.list varnames
#' @importFrom stats cor cov sd
#' @importFrom MASS mvrnorm 
#' @importFrom utils write.table
RunMCMC <- function(jags.model, 
                    params = NULL, 
                    name.list,
                    data.list, 
                    initial.list  = list(),
                    run.contrasts = FALSE,
                    use.contrast = "between",
                    contrasts = NULL,
                    run.ppp = FALSE,
                    k.ppp = 10,
                    n.data,
                    credible.region = 0.95, 
                    save.data = FALSE, 
                    ROPE = NULL,
                    merge.MCMC = FALSE, 
                    run.diag = FALSE,
                    param.diag = NULL,
                    sep = ",",
                    monochrome = TRUE,
                    plot.colors = c("#495054", "#e3e8ea"),
                    graphic.type = "pdf", 
                    plot.size = "15,10",
                    scaling = 100, 
                    plot.aspect = NULL, 
                    vector.graphic = FALSE, 
                    point.size = 12, 
                    font.type = "serif", 
                    one.file = TRUE, 
                    ppi = 300,
                    units = "in",
                    layout = "a4",
                    layout.inverse = FALSE,
                    ...
) {
  
  # Name list
  project.name <- name.list$project.name
  project.data <- name.list$project.data
  project.dir <- name.list$project.dir
  model.type <- name.list$model.type
  model.name <- name.list$model.name
  job.title <- name.list$job.title
  job.names <- name.list$job.names
  job.group <- name.list$job.group
  saved.steps = name.list$saved.steps
  thinned.steps = name.list$thinned.steps
  adapt.steps = name.list$adapt.steps
  burnin.steps = name.list$burnin.steps
  jags.seed <- name.list$jags.seed
  jags.method <- name.list$jags.method
  jags.chains <- name.list$jags.chains
  
  # Add jags seed to initial list
  initial.list <- c(initial.list, .RNG.seed = jags.seed)
      
  # Number of samples
  n.samples <- ceiling(saved.steps / jags.chains)
  
  # Display start time
  start.time <- Sys.time()
  cat(format(start.time,"\nStarted at %d.%m.%Y - %H:%M:%S\n"))
  
  # conduct JAGS
  if ( is.null(merge.MCMC) ) {
    
    # Initializing model
    jags.data <- runjags::run.jags( method = jags.method ,
                                    model= jags.model , 
                                    monitor = params , 
                                    data = data.list ,  
                                    inits = initial.list , 
                                    n.chains = jags.chains ,
                                    adapt = adapt.steps ,
                                    burnin = burnin.steps , 
                                    sample = n.samples ,
                                    thin = thinned.steps ,
                                    summarise = FALSE ,
                                    plots = FALSE 
    )
    
    # Generate random samples from the posterior distribution of monitored parameters
    # Resulting data.MCMC object has these indices:
    # data.MCMC[[ chainIdx ]][ stepIdx , paramIdx ]
    data.MCMC <- coda::as.mcmc.list(jags.data)
    
    # inspect merged MCMC chains
  } else {
    data.MCMC <- merge.MCMC
  }
  
  # Clean up working memory
  RemoveGarbage("jags.data")
  
  # Treat results as matrix for further examination
  matrix.MCMC <- as.matrix(data.MCMC)
  # Sort matrix by column names
  if (ncol(matrix.MCMC)>1) matrix.MCMC <- matrix.MCMC[, order(colnames(matrix.MCMC))]
  
  # Append bracket (ie., [1] ) for naming purposes
  colnames(matrix.MCMC) <- unlist(lapply(colnames(matrix.MCMC), function (x) if(regexpr('\\[', x)[1]<0) paste0(x,"[1]") else x ))
  coda::varnames(data.MCMC) <- unlist(lapply(coda::varnames(data.MCMC), function (x) if(regexpr('\\[', x)[1]<0) paste0(x,"[1]") else x ))
  
  # Diagnostics
  if (run.diag) {
    cat("\nConducting diagnostics. Please wait.\n")
    
    diag.start.time  <- Sys.time()
    if (length(param.diag)) {
      param.diag <- grep(paste(TrimSplit(param.diag),collapse="|"), 
                         coda::varnames(data.MCMC), value = TRUE)
    } else {
      param.diag <- coda::varnames(data.MCMC)
    }
    diag.length <- length(param.diag)
    diag.plots <- lapply(1:diag.length, function(i) { 
      
      x <- param.diag[i]    
      
      if (stats::sd(matrix.MCMC[,x]) > 0) {
        diag <- DiagMCMC(data.MCMC = data.MCMC, 
                         par.name = x, 
                         job.names = job.names, 
                         job.group = job.group, 
                         credible.region = credible.region
        )
      } else {
        diag <- (paste0(x , " appears to be a constant. No available diagnostics."))
      }
      
      ETA(diag.start.time , i , diag.length)
      
      return (diag)
      
    } )
    
    # Adding names to diagnostics 
    names(diag.plots) <- param.diag
    
    if (save.data) {
      cat("\nAdjusting and saving diagnostic plots. Please wait.\n")
      
      #  Save plots as PowerPoint, Default is raster graphics. 
      ## Change vector.graphic to TRUE if needed (not recommended) 
      ParsePlot(diag.plots[lapply(diag.plots,length)>1],
                project.dir = paste0(project.dir,"Diagnostics/"),
                project.name = project.name,
                graphic.type = graphic.type, 
                plot.size = plot.size,
                save.data = save.data, 
                vector.graphic = vector.graphic, 
                point.size = point.size, 
                font.type = font.type, 
                one.file = one.file,
                layout = layout,
      )
    }
  }
  
  # Add contrasts for selected models
  if (run.contrasts) {
    
    q.levels <- data.list$q.levels
    
    cat("\nComputing contrasts. Please wait.\n")
    
    # Createsum to zero contrasts for metric and nominal models
    if (model.type == "Metric" | model.type == "Nominal") {
      sum.zero <- SumToZero(q.levels, matrix.MCMC, contrasts)
    }
    
    # Add odds (and odds-ratios/cohen's d) for nominal models
    if (model.type == "Nominal") {
      
      # Create expected and observed nominal and proportions data
      expected.observed <- MatrixCombn(matrix.MCMC, 
                                       "o,o,e,e", 
                                       "NULL,p,NULL,p", 
                                       q.levels, 
                                       rm.last = FALSE, 
                                       row.means=FALSE)                                                                                        
      
      # Remove expected and observed columns from MCMC matrix
      matrix.MCMC <- matrix.MCMC[ , !colnames(matrix.MCMC) %in% colnames(expected.observed)]
      
      contrast.type <- c("b","o")
      contrast.data <- list(sum.zero, expected.observed)
      
    }
    
    # Add effect size cohen's d for metric model
    if (model.type == "Metric") {
      
      # Create mean difference data
      mean.diff <- MatrixCombn(matrix.MCMC, "m,s", q.levels = q.levels, rm.last = FALSE)
      
      # Remove mean difference columns from MCMC matrix
      matrix.MCMC <- matrix.MCMC[ , !colnames(matrix.MCMC) %in% colnames(mean.diff)]
      
      contrast.type <- c("b","m")
      contrast.data <- list(sum.zero, mean.diff)
    }
    
    # Run contrasts
    contrasts <- do.call(cbind, lapply(1:length(contrast.type), function (i) {
      RunContrasts(contrast.type[[i]], 
                   q.levels, 
                   use.contrast, 
                   contrasts, 
                   contrast.data[[i]], 
                   job.names)
    }))
  }
  
  # Add R^2 if regression
  if (model.type == "Regression") { 
      
    x <- data.list$x
    n.x <- data.list$n.x
    y <- data.list$y
    q <- data.list$q
    n <- data.list$n
    
    r.squared <- do.call(cbind,lapply(1:q, function (i) {
      zbeta <- matrix.MCMC[,grep(paste0("^zbeta$|^zbeta\\[",i),colnames(matrix.MCMC))]
      
      # Compute R^2
      cor.data <- stats::cor( x[,1:n.x[i]] , y  )  
      r.squared <- zbeta %*% matrix( cor.data , ncol=1 )
      # Compute robust R^2
      cor.data <- stats::cor( x[,1:n.x[i]] , y  , method="spearman")  
      rob.r.squared <- zbeta %*% matrix( cor.data , ncol=1 )
      # Compute adjusted R^2
      adjusted.r <- as.matrix(1 - (1-r.squared) * ( (n-1) / (n -(q+1)) ))
      # Compute adjusted robust R^2
      adjusted.rob.r <- as.matrix(1 - (1-rob.r.squared) * ( (n-1) / (n -(q+1)) ))
      
      # Add names
      colnames(r.squared) <- if (q>1) sprintf("R^2 (block: %s)",i) else "R^2"
      colnames(rob.r.squared) <- if (q>1) sprintf("Robust R^2 (block: %s)",i) else "Robust R^2"
      colnames(adjusted.r) <- if (q>1) sprintf("Adjusted R^2 (block: %s)",i) else "Adjusted R^2"
      colnames(adjusted.rob.r) <- if (q>1) sprintf("Adjusted Robust R^2 (block: %s)",i) else "Adjusted Robust R^2"
      
      # Return as matrix
      cbind(r.squared , rob.r.squared , adjusted.r , adjusted.rob.r)
      
    }))
  }  
  
  # Add PPP if SEM/CFA
  if (run.ppp) { 
    y <- data.list$y
    lat <- data.list$lat
    factor.seq <- data.list$factor.seq
    n <- data.list$n
    
    ppp.start.time  <- Sys.time()
    pppv <- 0
    jags.ppp <-  sample(1:nrow(matrix.MCMC), nrow(matrix.MCMC)/k.ppp)
    
    cov.mat <- stats::cov(y)
    cat("\nComputing PPP-value, please wait (it may take some time).\n")
    PPP <- lapply(1:length(jags.ppp), function (i) {   
      
      x <- matrix.MCMC[jags.ppp[i],]
      # Epsilon/Error variance matrix
      eps.pos <- grep("error", names(x))
      eps.length <- length(eps.pos)
      eps.matrix <- diag(x[eps.pos],eps.length)
      
      # Rho/Latent covariance matrix
      rho.pos <- grep("cov", names(x))
      rho.mat <- matrix(x[rho.pos], nrow=lat, ncol=lat, byrow=TRUE)
      
      # Lambda/Loading matrix
      lam.pos <- grep("lam", names(x))
      lam.length <- length(lam.pos)
      
      lambda <- do.call( rbind, lapply(1:lam.length, function (j) c(factor.seq[j], x[lam.pos][j]) ) )
      lam.matrix <- matrix( unlist ( lapply(1:lat, function (j) {
        if (j < max(lat)) {
          c ( lambda[ lambda[,1] == j , 2], rep(0,lam.length) ) 
        } else {
          lambda[ lambda[,1] == j , 2]
        }
      } ) ), lam.length, lat)
      
      # Calculate predicted covariance matrix
      pred.sigma <- lam.matrix %*% rho.mat %*% t(lam.matrix) + eps.matrix
      
      # Chi-square discrepancy (predicted data from model vs. covariance)
      pred.fit <- (n - 1) * (log(det(pred.sigma)) + sum(diag(solve(pred.sigma) %*% cov.mat )) - log(det(cov.mat)) - eps.length)
      
      # Simulate covariances from model
      sim.sigma <- cov ( MASS::mvrnorm(n=n, mu=rep(0,eps.length), Sigma=pred.sigma, empirical=FALSE) )
      
      # Chi-square discrepancy (simulated data from model vs. simulated covariance)
      sim.fit <- (n - 1) * (log(det(pred.sigma)) + sum(diag(solve(pred.sigma) %*% sim.sigma)) - log(det(sim.sigma)) - eps.length)
      
      # Compute PPP-value
      pppv <<- if (pred.fit < sim.fit) pppv + 1 else if (pred.fit == sim.fit) pppv + 0.5 else pppv
      PPP <- as.numeric(pppv / i)     
      
      ETA(ppp.start.time , i , length(jags.ppp))
      
      # Create matrix with chi-square, discrepancy between predicted and simulated data and PPP
      PPP <-  as.data.frame(t(
        c("Fit (Predicted)" = pred.fit, 
          "Fit (Simulated)" = sim.fit,
          "Fit (Discrepancy)" = (pred.fit-sim.fit),
          "PPP" = PPP)
      ))
      
      return (PPP)
      
    } )
    
    if (requireNamespace("plyr", quietly = TRUE)) {
      PPP <-  plyr::rbind.fill(PPP)
    } else {
      PPP <-  do.call(rbind,PPP)
    }
  }
  
  # create list of matrices to summarize
  list.MCMC <- list(
    matrix = matrix.MCMC,
    PPP = if (exists("PPP")) PPP,
    r.squared = if (exists("r.squared")) r.squared,
    sum.zero = if (exists("sum.zero")) sum.zero,
    count.data = if (exists("expected.observed")) expected.observed,
    mean.difference = if (exists("mean.diff")) mean.diff,
    contrasts = if (exists("contrasts")) contrasts
  )
  
  # Remove empty plots
  list.MCMC <- Filter(length, list.MCMC)
  
  summary.MCMC <- do.call(rbind,lapply(1:length(list.MCMC), function (k) {
    
    # Find params from MCMC list
    params <- colnames(list.MCMC[[k]])
    
    # Create final posterior parameter indicies
    summary.start.time <- Sys.time()
    summary.cat <- sprintf("\nSummarizing data for each parameter in %s. Please wait.\n" , 
                           gsub("\\."," ",names(list.MCMC)[[k]]) )
    cat(summary.cat)
    do.call(rbind,lapply(1:length(params), function(i) {
      
      summary <- SumMCMC( par = list.MCMC[[k]][, params[i]] , 
                          par.names = params[i], 
                          job.names = job.names, 
                          job.group = job.group,
                          n.data = n.data,
                          credible.region = credible.region, 
                          ROPE = ROPE
      )
      
      ETA(summary.start.time , i , length(params))
      
      return (summary)
      
    })) 
  }))
    
  # Display completion and running time
  stop.time <- Sys.time()
  total.time <- capture.output(difftime(stop.time, start.time))
  cat(format(stop.time,"\nCompleted at %d.%m.%Y - %H:%M:%S\n"), 
      gsub("Time difference of","Running time:",total.time),"\n", sep="")
  
  # Create MCMC and summary list (without chain information)
  final.MCMC <- list( raw.MCMC = data.MCMC, 
                      matrix.MCMC = do.call(cbind,list.MCMC),
                      summary.MCMC = summary.MCMC, 
                      name.list = name.list,
                      initial.list = initial.list, 
                      data.list = data.list, 
                      jags.model = jags.model,
                      run.time = c(start.time,stop.time)
  )
  
  # If applicable, add diag plots
  if (run.diag & !save.data) final.MCMC <- c(final.MCMC, diag = list(diag.plots))
  
  # Save data if requested
  if (save.data) {  
    cat("\nSaving data. Please wait.\n")
    # If requested, save summary as csv 
    data.file.name <- RemoveSpaces(paste0(project.dir,project.name,".csv"))
    utils::write.table(summary.MCMC, data.file.name, sep = sep)
    
    # Save final MCMC
    MCMC.file.name <- paste0(project.dir,"MCMC/",project.name,".rds")
    saveRDS(  final.MCMC , file = MCMC.file.name, compress="bzip2")
    
    # Create meta data
    meta.final.MCMC <- list(summary.MCMC = summary.MCMC, 
                            name.list = name.list,
                            initial.list = initial.list, 
                            data.list = data.list, 
                            jags.model = jags.model,
                            run.time = c(start.time,stop.time)
    )
    
    # Save meta data
    MCMC.file.name <- sub('-', '-META-', MCMC.file.name)
    saveRDS(  meta.final.MCMC , file = MCMC.file.name, compress="bzip2")
    
    # Append to final MCMC list
    final.MCMC <- c(final.MCMC, 
                    data.file.name = data.file.name,
                    MCMC.file.name = MCMC.file.name)
  }  
  
  # Clear up working memory
  RemoveGarbage("data.MCMC,list.MCMC")
  
  return (final.MCMC)
}
