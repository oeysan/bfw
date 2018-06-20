#' @title Run MCMC
#' @description Conduct MCMC simulations using JAGS
#' @param jags.model specify which module to use
#' @param params define parameters to observe, Default: NULL
#' @param name.list list of names
#' @param data.list list of data
#' @param initial.list initial values for analysis, Default: list()
#' @param saved.steps define the number of iterations/steps/chains in the MCMC simulations, Default: 10000
#' @param thinned.steps save every kth step of the original saved.steps, Default: 1
#' @param run.contrasts logical, indicating whether or not to run contrasts, Default: FALSE
#' @param use.contrast choose from "between", "within" and "mixed". Between compare groups at different conditions. Within compare a group at different conditions. Mixed compute all comparisons. 
#' @param contrasts define contrasts to use for analysis (defaults to all) , Default: NULL
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
#'  \code{\link[robust]{covRob}}
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
                    saved.steps, 
                    thinned.steps,
                    run.contrasts,
                    use.contrast,
                    contrasts,
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
  
  # Number of saved steps
  saved.steps <- saved.steps
  # Number of thinned steps (keep only every kth step in chain)
  thinned.steps <- thinned.steps
  # Number of adapting steps
  adapt.steps <- max ( (saved.steps * 5) / 100 , 2000 )
  # Number of burn-in steps
  burnin.steps <- max ( (saved.steps * 7.5) / 100 , 3000 )
  
  # Runjags options
  # Disable redundant warnings
  try( runjags::runjags.options( inits.warning=FALSE , rng.warning=FALSE ) )
  # Set runjags method and number of chains
  detect.cores <- parallel::detectCores()
  jags.method <- if ( !is.finite(detect.cores || detect.cores < 4) ) "rJags" else "parallel"
  n.chains <- if ( detect.cores >= 4 ) 4 else detect.cores
  
  # Number of samples
  n.samples <- ceiling(saved.steps / n.chains)
  
  # Name list
  project.name <- name.list$project.name
  project.data <- name.list$project.data
  project.dir <- name.list$project.dir
  model.type <- name.list$model.type
  model.name <- name.list$model.name
  job.title <- name.list$job.title
  job.names <- name.list$job.names
  job.group <- name.list$job.group
  jags.seed <- name.list$jags.seed
  initial.list <- c(initial.list, .RNG.seed = jags.seed)
  
  # Display start time
  start.time <- Sys.time()
  cat("\n",format(start.time, "Started at %d.%m.%Y - %H:%M:%S"),"\n")
  
  # conduct JAGS
  if ( is.null(merge.MCMC) ) {
    
    # Initializing model
    jags.data <- runjags::run.jags( method = jags.method ,
                                    model= jags.model , 
                                    monitor = params , 
                                    data = data.list ,  
                                    inits = initial.list , 
                                    n.chains = n.chains ,
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
  
  # Add sum to zero if predict metric/nominal
  if (run.contrasts) {
    
    q.levels <- data.list$q.levels
    
    cat("\nCalculating contrasts, please wait (it may take some time).\n")
    
    if (model.type == "Metric" | model.type == "Nominal") {
      matrix.MCMC <- cbind(matrix.MCMC,SumToZero(q.levels, matrix.MCMC, contrasts))
    }
    if (model.type == "Nominal") {
      if (length(q.levels)>1) matrix.MCMC <- cbind( matrix.MCMC, MatrixCombn(matrix.MCMC,"o",q.levels,row.means=FALSE) )
      contrast.type <- "b,o"
    }
    if (model.type == "Metric") {
      if (length(q.levels)>1) matrix.MCMC <- cbind( matrix.MCMC, MatrixCombn(matrix.MCMC,"m,s",q.levels) )
      contrast.type <- "b,m"
    }
    
    matrix.MCMC <- cbind( matrix.MCMC , 
                          RunContrasts(contrast.type , q.levels , use.contrast , contrasts , matrix.MCMC , job.names) )
  }
  
  # Add R^2 if regression
  if (model.type == "Regression") { 
    
    x <- data.list$x
    n.x <- data.list$n.x
    y <- data.list$y
    q <- data.list$q
    
    r.squared <- lapply(1:q, function (i) {
      zbeta <- matrix.MCMC[,grep(paste0("^zbeta$|^zbeta\\[",i),colnames(matrix.MCMC))]
      
      if (name.list$robust) {
        if (!requireNamespace("robust", quietly = TRUE)) { 
          warning("Package \"robust\" needed to run robust, frequntic covariate estimates.") 
        }
        cor.data <- robust::covRob( data.frame( x[,1:n.x[i]] , y ), 
                                    corr = TRUE, 
                                    ntrial = 50000)$cov[2]          
      } else {
        cor.data <- stats::cor( x[,1:n.x[i]] , y )
      }      
      r.squared <- zbeta %*% matrix( cor.data , ncol=1 )
      colnames(r.squared) <- sprintf("R^2 (block: %s)",i)
      
      return (r.squared)
    })
    matrix.MCMC <-  cbind(matrix.MCMC, do.call(cbind, r.squared))
  }
  
  # Add PPP if SEM/CFA
  if (model.type == "observed" & !name.list$robust) { 
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
      
      eta <- Sys.time() + ( (nrow(matrix.MCMC) - i) * ( (Sys.time() - ppp.start.time) / i ) )
      cat("\r" , 
          sprintf("Progress: %.02f%% (PPP-value: %.03f). ETA: %s ", 
                  (i * 100) / nrow(matrix.MCMC),
                  PPP,
                  format(eta,"%d.%m.%Y - %H:%M:%S")
          )
      )
      
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
  summary.MCMC <- do.call(rbind,lapply(1:length(params), function(i) {
    
    SumMCMC( par = matrix.MCMC[, params[i]] , 
             par.names = params[i], 
             job.names = job.names, 
             job.group = job.group,
             n.data = n.data,
             credible.region = credible.region, 
             ROPE = ROPE
    )
  } ) ) 
  
  # Diagnostics
  if (run.diag) {
    cat("\nConducting diagnostics, please wait (it may take some time).\n")
    diag.start.time  <- Sys.time()
    diag.length <- length(coda::varnames(data.MCMC))
    diag.plots <- lapply(1:diag.length, function(i) { 
      
      x <- coda::varnames(data.MCMC)[i]    
      
      if (stats::sd(matrix.MCMC[,x]) == 0) {
        cat("\n" , x , "appears to be a constant. Skipping diagnostics.\n")
        return (NULL)
      } else {
        diag <- DiagMCMC(data.MCMC = data.MCMC, 
                         par.name = x, 
                         job.names = job.names, 
                         job.group = job.group, 
                         project.name = project.name, 
                         credible.region = credible.region, 
                         save.data = save.data, 
                         project.dir = project.dir,
                         monochrome = monochrome,
                         plot.colors = plot.colors,
                         font.type = font.type      
        )
        
        eta <- Sys.time() + ( (diag.length - i) * ( (Sys.time() - diag.start.time) / i ) ) 
        cat("\r", 
            sprintf("Progress: %.02f%% (parameter: %s/%s). ETA: %s ", 
                    (i * 100) / diag.length,
                    i,
                    diag.length,
                    format(eta,"%d.%m.%Y - %H:%M:%S")
            )
        )
      }
      
      return (diag)
    } )
    
    if (save.data) {
      cat("\nSaving diagnostics. Please wait.\n")
      # Remove empty elements from list
      diag.plots <- Filter(length, diag.plots)
      
      #  Save plots as PowerPoint, Default is raster graphics. 
      ## Change vector.graphic to TRUE if needed (not recommended) 
      ParsePlot(diag.plots,
                project.dir = paste0(project.dir,"Diagnostics/"),
                project.name = project.name,
                graphic.type = "pptx", 
                plot.size = "15,10",
                save.data = save.data, 
                vector.graphic = vector.graphic, 
                point.size = 15, 
                font.type = "serif", 
                one.file = TRUE,
                layout = "pw",
      )
    }
  }
  
  # Display completion and running time
  stop.time <- Sys.time()
  total.time <- capture.output(difftime(stop.time, start.time))
  cat("\n",format(stop.time, "Completed at %d.%m.%Y - %H:%M:%S"),
      "\n",gsub("Time difference of","Running time:",total.time),"\n\n")
  
  # Create MCMC and summary list (without chain information)
  final.MCMC <- list( raw.MCMC = data.MCMC, 
                      matrix.MCMC = matrix.MCMC,
                      summary.MCMC = summary.MCMC, 
                      name.list = name.list,
                      data.list = data.list, 
                      jags.model = jags.model,
                      run.time = c(start.time,stop.time)
  )
  
  
  # Save if requsted
  if (save.data) {  
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
