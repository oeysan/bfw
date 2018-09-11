#' @title Run MCMC
#' @description Conduct MCMC simulations using JAGS
#' @param jags.model specify which module to use
#' @param params define parameters to observe, Default: NULL
#' @param name.list list of names
#' @param data.list list of data
#' @param initial.list initial values for analysis, Default: list()
#' @param run.contrasts logical, indicating whether or not to run contrasts, Default: FALSE
#' @param use.contrast choose from "between", "within" and "mixed". Between compare groups at different conditions. Within compare a group at different conditions. Mixed compute all comparisons. 
#' @param contrasts define contrasts to use for analysis (defaults to all) , Default: NULL
#' @param run.ppp logical, indicating whether or not to conduct ppp analysis, Default: FALSE
#' @param n.data sample size for each parameter
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
#' @param ROPE define range for region of practical equivalence (e.g., c(-0.05 , 0.05), Default: NULL
#' @param merge.MCMC logical, indicating whether or not to merge MCMC chains, Default: FALSE
#' @param run.diag logical, indicating whether or not to run diagnostics, Default: FALSE
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param monochrome logical, indicating whether or not to use monochrome colors, else use \link[bfw]{DistinctColors}, Default: TRUE
#' @param plot.colors range of color to use, Default: c("#495054", "#e3e8ea")
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
                    params, 
                    name.list,
                    data.list, 
                    initial.list,
                    run.contrasts,
                    use.contrast,
                    contrasts,
                    run.ppp,
                    n.data,
                    credible.region, 
                    save.data = FALSE, 
                    ROPE,
                    merge.MCMC, 
                    run.diag,
                    sep,
                    monochrome = TRUE,
                    plot.colors = c("#495054", "#e3e8ea"),
                    graphic.type = "png", 
                    plot.size = "15,10",
                    scaling = 100, 
                    plot.aspect = NULL, 
                    vector.graphic = TRUE, 
                    point.size = 15, 
                    font.type = "serif", 
                    one.file = TRUE, 
                    ppi = 300,
                    units = "in",
                    layout = "pw",
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
  
  # Number of adapting steps
  if (!length(adapt.steps)) adapt.steps <- max ( (saved.steps * 5) / 100 , 2000 )
  # Number of burn-in steps
  if (!length(burnin.steps)) burnin.steps <- max ( (saved.steps * 7.5) / 100 , 3000 )
  
  # Runjags options
  # Disable redundant warnings
  try( runjags::runjags.options( inits.warning=FALSE , rng.warning=FALSE ) )
  # Set runjags method and number of chains
  if (!length(jags.method)) {
    detect.cores <- parallel::detectCores()
    jags.method <- if ( !is.finite(detect.cores || detect.cores < 4) ) "simple" else "parallel"
  }
  if (!length(jags.chains)) {
    jags.chains <- if ( detect.cores >= 4 ) 4 else detect.cores
  }
  
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
  
  # Treat results as matrix for further examination
  matrix.MCMC <- as.matrix(data.MCMC)
  
  # Append bracket (ie., [1] ) for naming purposes
  colnames(matrix.MCMC) <- unlist(lapply(colnames(matrix.MCMC), function (x) if(regexpr('\\[', x)[1]<0) paste0(x,"[1]") else x ))
  coda::varnames(data.MCMC) <- unlist(lapply(coda::varnames(data.MCMC), function (x) if(regexpr('\\[', x)[1]<0) paste0(x,"[1]") else x ))
  
  # Diagnostics
  if (run.diag) {
    cat("\nConducting diagnostics. Please wait.\n")
    diag.start.time  <- Sys.time()
    diag.length <- length(coda::varnames(data.MCMC))
    diag.plots <- lapply(1:diag.length, function(i) { 
      
      x <- coda::varnames(data.MCMC)[i]    

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
    names(diag.plots) <- coda::varnames(data.MCMC)
    
    if (save.data) {
      cat("\nAdjusting and saving diagnostic plots. Please wait.\n")
      
      diag.plots <- diag.plots[lapply(diag.plots,length)>1]

      #  Save plots as PowerPoint, Default is raster graphics. 
      ## Change vector.graphic to TRUE if needed (not recommended) 
      ParsePlot(diag.plots,
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
    
    # Createsum to zero contrasts for mettric and nominal models
    if (model.type == "Metric" | model.type == "Nominal") {
      sum.zero <- SumToZero(q.levels, matrix.MCMC, contrasts)
      matrix.MCMC <- cbind(matrix.MCMC, sum.zero)
    }
    
    # Add odds (and odds-ratios/cohen's d) for nominal models
    if (model.type == "Nominal") {
        
      contrast.type <- c("b","o")
      # Create matrix combiantions for models with multiple factors
      if (length(q.levels)>1) { 
        odds <- MatrixCombn(matrix.MCMC, "o", q.levels, row.means=FALSE)
        matrix.MCMC <- cbind(matrix.MCMC, odds)
      }                                                                                        
    }
    # Add effect size cohen's d for metric model
    if (model.type == "Metric") {
      contrast.type <- c("b","m")
      # Create matrix combiantions for models with multiple factors
      if (length(q.levels)>1) {
        mean.diff <- MatrixCombn(matrix.MCMC, "m,s",  q.levels)
        matrix.MCMC <- cbind(matrix.MCMC, mean.diff)
      }
    }
        
    # Run contrasts and add contrasts to data matrix
    done.contrasts <- do.call(cbind, lapply(contrast.type, function (x) {
      RunContrasts(x , q.levels , use.contrast , contrasts , matrix.MCMC , job.names) 
    }))
    matrix.MCMC <- cbind(matrix.MCMC, done.contrasts)
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
      colnames(r.squared) <- sprintf("R^2 (block: %s)",i)
      colnames(rob.r.squared) <- sprintf("Robust R^2 (block: %s)",i)
      colnames(adjusted.r) <- sprintf("Adjusted R^2 (block: %s)",i)
      colnames(adjusted.rob.r) <- sprintf("Adjusted Robust R^2 (block: %s)",i)

      # Return as matrix
      cbind(r.squared , rob.r.squared , adjusted.r , adjusted.rob.r)
      
    }))
      
    # Add R^2 to MCMC matrix  
    matrix.MCMC <-  cbind(matrix.MCMC, r.squared)
    
  }  
  
  # Add PPP if SEM/CFA
  if (run.ppp) { 
    y <- data.list$y
    lat <- data.list$lat
    factor.seq <- data.list$factor.seq
    n <- data.list$n
    
    ppp.start.time  <- Sys.time()
    cov.mat <- stats::cov(y)
    pppv <- 0
    cat("\nComputing PPP-value, please wait (it may take some time).\n")
    PPP <- lapply(1:nrow(matrix.MCMC), function (i) {   
      
      x <- matrix.MCMC[i,]
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
      
      lambda <- do.call( rbind, lapply(1:lam.length, function (i) c(factor.seq[i], x[lam.pos][i]) ) )
      lam.matrix <- matrix( unlist ( lapply(1:lat, function (i) {
        if (i < max(lat)) {
          c ( lambda[ lambda[,1] == i , 2], rep(0,lam.length) ) 
        } else {
          lambda[ lambda[,1] == i , 2]
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
      if (pred.fit <= sim.fit) pppv <<- pppv + 1
      PPP <- as.numeric(pppv / i)     
      
      ETA(ppp.start.time , i , nrow(matrix.MCMC))

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
      matrix.MCMC <-  cbind(matrix.MCMC, plyr::rbind.fill(PPP))
    } else {
      matrix.MCMC <-  cbind(matrix.MCMC, do.call(rbind,PPP))
    }
  }
  
  # Find params from MCMC list
  params <- colnames(matrix.MCMC)
  
  # Create final posterior parameter indicies
  summary.start.time <- Sys.time()
  cat("\nSummarizing data of each parameter. Please wait.\n")
  summary.MCMC <- do.call(rbind,lapply(1:length(params), function(i) {
    
  summary <- SumMCMC( par = matrix.MCMC[, params[i]] , 
                      par.names = params[i], 
                      job.names = job.names, 
                      job.group = job.group,
                      n.data = n.data,
                      credible.region = credible.region, 
                      ROPE = ROPE
  )
    
  ETA(summary.start.time , i , length(params))

  return (summary)
      
  } ) ) 
        
  # Display completion and running time
  stop.time <- Sys.time()
  total.time <- capture.output(difftime(stop.time, start.time))
  cat(format(stop.time,"\nCompleted at %d.%m.%Y - %H:%M:%S\n"), 
      gsub("Time difference of","Running time:",total.time),"\n", sep="")
        
  # Create MCMC and summary list (without chain information)
  final.MCMC <- list( raw.MCMC = data.MCMC, 
                      matrix.MCMC = matrix.MCMC,
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
    
    # Append to final MCMC list
    final.MCMC <- c(final.MCMC, 
                    data.file.name = data.file.name,
                    MCMC.file.name = MCMC.file.name)
  }  
      
  return (final.MCMC)
}
