#' @title Settings
#' @description main settings for bfw
#' @param y criterion variable(s), Default: NULL
#' @param y.names optional names for criterion variable(s), Default: NULL
#' @param x predictor variable(s), Default: NULL
#' @param x.names optional names for predictor variable(s), Default: NULL
#' @param latent latent variables, Default: NULL
#' @param latent.names optional names for for latent variables, Default: NULL
#' @param observed observed variable(s), Default: NULL
#' @param observed.names optional names for for observed variable(s), Default: NULL
#' @param additional supplemental parameters for fitted data (e.g., indirect pathways and total effect), Default: NULL
#' @param additional.names optional names for supplemental parameters, Default: NULL
#' @param x.steps define number of steps in hierarchical regression , Default: NULL
#' @param x.blocks define which predictors are included in each step (e.g., for three steps "1,2,3") , Default: NULL
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
#' @param credible.region summarize uncertainty by defining a region of most credible values (e.g., 95 percent of the distribution), Default: 0.95
#' @param ROPE define range for region of practical equivalence (e.g., c(-0.05 , 0.05), Default: NULL
#' @param run.contrasts logical, indicating whether or not to run contrasts, Default: FALSE
#' @param use.contrast choose from "between", "within" and "mixed". Between compare groups at different conditions. Within compare a group at different conditions. Mixed compute all comparisons
#' @param contrasts define contrasts to use for analysis (defaults to all) , Default: NULL
#' @param run.ppp logical, indicating whether or not to conduct ppp analysis, Default: FALSE
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
#' @param silent logical, indicating whether or not analysis should be run silent, Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @return data from MCMC \link[bfw]{RunMCMC}
#' @details Settings act like the main framework for bfw, connecting function, model and JAGS.
#' @seealso
#'  \code{\link[utils]{tail}},\code{\link[utils]{modifyList}},\code{\link[utils]{capture.output}}
#' @rdname bfw
#' @export
#' @importFrom utils tail modifyList capture.output
bfw <- function(y = NULL,
                y.names = NULL,
                x = NULL,
                x.names = NULL,
                latent = NULL,
                latent.names = NULL,
                observed = NULL,
                observed.names = NULL,
                additional = NULL,
                additional.names = NULL,
                x.steps = NULL,
                x.blocks = NULL,
                job.title = NULL,
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
                credible.region = 0.95,
                ROPE = NULL,
                run.contrasts = FALSE,
                use.contrast = "between",
                contrasts = NULL,
                run.ppp = FALSE,
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
                monochrome = TRUE,
                plot.colors = c("#495054", "#e3e8ea"),
                graphic.type = "pptx",
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
                silent = FALSE,
                ...
) {

  # Directory of bfw
  bfw.dir <- find.package("bfw", lib.loc=NULL, quiet = TRUE)

  # If necessary add trailing slash to project directory
  if ( tail(TrimSplit(project.dir,""),1) != "/") {
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
  # Create argument list
  model.arguments  <- paste0(paste(model.arguments,
                                   model.arguments,sep="="),
                             collapse=",")
  # Create data list from model from specified argument list
  stats.model <- eval(parse(text=sprintf("stats.model(%s)" , model.arguments)))

  # assign attributes from stats.model
  for (i in 1:length(stats.model)){
    assign( names(stats.model)[i] , stats.model[[i]] )
  }

  # Create seed if seed is not set
  if (is.null(jags.seed)) jags.seed <- sample(1:10^6,1)

  # Create save name
  project.name <- FileName( project.name , data.set , model.type , job.title , time.stamp)
  
  # Tidy up JAGS model
  jags.model <- TidyCode(jags.model)

  # Create name list
  tmp.list <- list(
    project.name = project.name,
    project.dir = project.dir,
    project.data = project.data,
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
    jags.method <- jags.method,
    jags.chains <- jags.chains
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
                    run.contrasts = run.contrasts,
                    use.contrast = use.contrast,
                    contrasts = contrasts,
                    run.ppp = run.ppp,
                    n.data = n.data,
                    credible.region = credible.region,
                    ROPE = ROPE,
                    merge.MCMC = merge.MCMC,
                    run.diag = run.diag,
                    sep = sep,
                    monochrome = monochrome,
                    plot.colors = plot.colors,
                    graphic.type = graphic.type,
                    plot.size = plot.size,
                    scaling = scaling,
                    plot.aspect = plot.aspect,
                    save.data = save.data,
                    vector.graphic = vector.graphic,
                    point.size = point.size,
                    font.type = font.type,
                    one.file = one.file,
                    ppi = 300,
                    units = units,
                    layout = layout,
                    layout.inverse = layout.inverse)

  # Run MCMC
  if (silent) {
    invisible(utils::capture.output( final.MCMC <- do.call( RunMCMC , MCMC.list) ))
  } else {
    final.MCMC <- do.call( RunMCMC , MCMC.list)
  }

  return (final.MCMC)
}
