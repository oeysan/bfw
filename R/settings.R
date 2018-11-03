#' @title Settings
#' @description main settings for bfw
#' @param job.title title of analysis, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param jags.model specify which module to use
#' @param jags.seed specify seed to replicate a analysis, Default: NULL
#' @param jags.method specify method for JAGS (e.g., parallel or simple), Default: NULL
#' @param jags.chains specify specify number of chains for JAGS, Default: NULL
#' @param custom.function custom function to use (e.g., defined function, external R file or string with function), Default: NULL
#' @param custom.model define a custom model to use (e.g., string or text file (.txt), Default: NULL
#' @param params define parameters to observe, Default: NULL
#' @param saved.steps define the number of iterations/steps/chains in the MCMC simulations, Default: 10000
#' @param thinned.steps save every kth step of the original saved.steps, Default: 1
#' @param adapt.steps the number of adaptive iterations to use at the start of each simulation, Default: NULL
#' @param burnin.steps the number of burnin iterations, NOT including the adaptive iterations to use for the simulation, Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param project.name name of project, Default: 'Project'
#' @param project.dir define where to save data, Default: 'Results/'
#' @param project.data define data to use for analysis (e.g., csv, rda, custom data.frame or matrix, or data included in package, Default: NULL
#' @param time.stamp logical, indicating whether or not to append unix time stamp to file name, Default: TRUE
#' @param save.data logical, indicating whether or not to save data, Default: FALSE
#' @param data.set define subset of data, Default: 'AllData'
#' @param data.format define what data format is being used, Default: 'csv'
#' @param raw.data logical, indicating whether or not to use unprocessed data, Default: FALSE
#' @param run.robust logical, indicating whether or not robust analysis, Default: FALSE
#' @param merge.MCMC logical, indicating whether or not to merge MCMC chains, Default: FALSE
#' @param run.diag logical, indicating whether or not to run diagnostics, Default: FALSE
#' @param sep symbol to separate data (e.g., comma-delimited), Default: ','
#' @param silent logical, indicating whether or not to run analysis without output, Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @return data from MCMC \link[bfw]{RunMCMC}
#' @details Settings act like the main framework for bfw, connecting function, model and JAGS.
#' @seealso
#'  \code{\link[utils]{head}},\code{\link[utils]{modifyList}},\code{\link[utils]{capture.output}}
#' @rdname bfw
#' @export
#' @importFrom utils tail modifyList capture.output
bfw <- function(job.title = NULL,
                job.group = NULL,
                jags.model,
                jags.seed = NULL,
                jags.method = NULL,
                jags.chains = NULL,
                custom.function = NULL,
                custom.model = NULL,
                params = NULL,
                saved.steps = 10000,
                thinned.steps = 1,
                adapt.steps = NULL,
                burnin.steps = NULL,
                initial.list = list(),
                project.name = "Project",
                project.dir = "Results/",
                project.data = NULL,
                time.stamp = TRUE,
                save.data = FALSE,
                data.set = "AllData",
                data.format = "csv",
                raw.data = FALSE,
                run.robust = FALSE,
                merge.MCMC = FALSE,
                run.diag = FALSE,
                sep = ",",
                silent = FALSE,
                ...
) {

  # Directory of bfw
  bfw.dir <- find.package("bfw", lib.loc=NULL, quiet = TRUE)

  # If necessary add trailing slash to project directory
  if ( utils::tail(TrimSplit(project.dir,""),1) != "/") {
    project.dir <- paste0(project.dir,"/")
  }

  # Some initial definitions
  n.data <- NULL
  data.list <- NULL
  job.names <- NULL

  # Select data (custom, processed or raw data) or else stop
  project.data.extension <- tolower(utils::tail(TrimSplit(project.data, sep="\\."),1))
  if (project.data.extension == "csv") {
    DF <- read.csv(project.data, header = TRUE, sep = sep)
  } else if (inherits(project.data, "data.frame") | inherits(project.data, "matrix")) {
    DF <- as.data.frame(project.data)
  } else if (project.data.extension == "rda") {
    DF <- load(project.data)
  } else if (project.data %in% gsub(".rda", "" , list.files(paste0(bfw.dir, "/data")))) {
    if (!raw.data) {
      DF <- eval(parse(text=paste0("bfw::",project.data)))
    } else {
      DF <- ReadFile(file = project.data, path = project.name, type ="data", data.format = data.format)
    }
  } else {
    stop("Please specify data. Quitting.")

  }

  if (save.data) {
    # Create directories
    make.dirs <- c("MCMC","Diagnostics")
    invisible(lapply(make.dirs, function(x) {
      make.dir <- paste0(project.dir,x)
      if (!dir.exists(make.dir)) {
        dir.create(make.dir,recursive = TRUE)
      }
    }))
  }

  # Select sample
  data.sets <- if("dataset" %in% colnames(DF)) unique(DF$dataset) else NULL
  if ( any( tolower(data.sets) %in% tolower(data.set) ) ) {
    data.set <- toString ( data.sets[ which ( tolower(data.sets) %in% tolower(data.set) ) ] )
    DF <- DF[DF$dataset == data.set, ]
  }

  # If custom function
  if (length(custom.function)) {
    model.type <- "Custom function"
    if (is.function(custom.function)) {
      stats.model <- custom.function
    } else if (toupper(utils::tail(TrimSplit(custom.function,".", fixed = TRUE),1)) == "R") {
      stats.model <- source(custom.function)[[1]]
    } else if (SingleString(custom.function)) {
      stats.model <- eval(parse(text=custom.function))
    }
  } else {
    # Define model type
    model.type <- RemoveSpaces(CapWords(jags.model,TRUE))
    # Select model function
    stats.model <- eval(parse(text=paste0("Stats",model.type)))
  }
  
  # If custom jags model
  if (length(custom.model)) {
    model.name <- paste0("Custom JAGS model")
    if (tolower(utils::tail(TrimSplit(custom.model,".", fixed = TRUE),1)) == "txt") {
      jags.model <- paste(readLines(custom.model,warn=FALSE), collapse="\n")
    } else if (SingleString(custom.model)) {
      jags.model <- custom.model
    }
  } else {
    # Specify model type
    jags.model <- paste0("stats_",RemoveSpaces(tolower(jags.model)))
    # Check if model exists
    list.models <- list.files(paste0(system.file(package = 'bfw'),"/extdata/models/"))
    if (!length(grep(jags.model,list.models))) stop("The model: ", model.name, " does not exist. Quitting.")
    # Specify model type (robust)
    if (run.robust) jags.model <- paste0(jags.model,"_robust")
    # Final model name
    model.name <- jags.model
    # Select specified model
    jags.model <- ReadFile( model.name , data.format = "txt" )

  }
  
  # Get arguments from model function
  model.arguments  <- TrimSplit(names(formals(stats.model)))

  # Create argument list not defined by user
  model.arguments <- paste0(unlist(lapply(model.arguments, function (arg) {
    if (exists(arg) & arg != "...") sprintf("%s = %s", arg , arg)
  })), collapse = ",")  
                        
  # Create data list from model from specified argument list
  stats.model <- eval(parse(text=sprintf("stats.model(%s , ...)" , model.arguments)))

  # assign attributes from stats.model
  for (i in 1:length(stats.model)){
    assign( names(stats.model)[i] , stats.model[[i]] )
  }

  # Create seed if seed is not set
  if (is.null(jags.seed)) jags.seed <- sample(1:10^6,1)

  # Create save name
  if (run.robust) {
    job.title <- if (is.null(job.title)) "Robust" else paste0(job.title,"-Robust")
  }
  project.name <- FileName( project.name , data.set , model.type , job.title , time.stamp)
  
  # Tidy up JAGS model
  jags.model <- TidyCode(jags.model)
  
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

  # Create name list
  tmp.list <- list(
    project.name = project.name,
    project.dir = project.dir,
    project.data = project.data,
    data.set = data.set,
    job.title = job.title,
    job.names = job.names,
    job.group = job.group,
    model.name = model.name,
    model.type = model.type,
    robust = run.robust,
    saved.steps = saved.steps,
    thinned.steps = thinned.steps,
    adapt.steps = adapt.steps,
    burnin.steps = burnin.steps,
    jags.model = jags.model,
    jags.seed = jags.seed,
    jags.method = jags.method,
    jags.chains = jags.chains
  )
  
  # Update name list with name list from function
  name.list <- if (exists("name.list")) utils::modifyList(tmp.list,name.list) else tmp.list

  # Determine whether to use normal or combined MCMC chains
  merge.MCMC <- if (merge.MCMC) MergeMCMC( model.type , project.dir , data.sets ) else NULL
  
  # Store arguments for MCMC run
  MCMC.list <- list(jags.model = jags.model,
                    params = params,
                    name.list = name.list,
                    data.list = data.list,
                    initial.list = initial.list,
                    saved.steps = saved.steps,
                    thinned.steps = thinned.steps,
                    n.data = n.data,
                    merge.MCMC = merge.MCMC,
                    run.diag = run.diag,
                    sep = sep,
                    save.data = save.data,
                    ...)

  # Run MCMC
  if (silent) {
    invisible(utils::capture.output( final.MCMC <- do.call( RunMCMC , MCMC.list) ))
  } else {
    final.MCMC <- do.call( RunMCMC , MCMC.list)
  }

  return (final.MCMC)
}
