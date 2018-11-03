#' @title Regression
#' @description Simple, multiple and hierarchical regression
#' @param y criterion variable(s), Default: NULL
#' @param y.names optional names for criterion variable(s), Default: NULL
#' @param x predictor variable(s), Default: NULL
#' @param x.names optional names for predictor variable(s), Default: NULL
#' @param x.steps define number of steps in hierarchical regression , Default: NULL
#' @param x.blocks define which predictors are included in each step (e.g., for three steps "1,2,3") , Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param ... further arguments passed to or from other methods
#' @seealso
#'  \code{\link[stats]{complete.cases}}
#' @rdname StatsRegression
#' @export
#' @importFrom stats complete.cases
StatsRegression <- function(y = NULL,
                           y.names = NULL,
                           x = NULL,
                           x.names = NULL,
                           x.steps = NULL,
                           x.blocks = NULL,
                           DF,
                           params = NULL,
                           job.group = NULL,
                           initial.list = list(),
                           ...
) {


  # Select continous/dichotomous/dummy paramaters as predictors (and remove non-complete cases across parameters)
  x <- TrimSplit(x)
  DF <- DF[stats::complete.cases(DF[, c(y,x)]), ]
  x.matrix <- as.matrix(sapply(DF[, x], as.numeric))

  # Select continous paramater as criterion
  y.matrix <- as.numeric(DF[, y])

  # Number of datapoints
  n <- dim(x.matrix)[1]

  # Number of steps
  x.steps <- if (is.null(x.steps)) 1 else as.numeric(x.steps)

  # Number of variables per block
  x.blocks <- if (is.null(x.blocks)) dim(x.matrix)[2] else as.numeric(TrimSplit(x.blocks))

  # Create job.names
  y.names <- if (!is.null(y.names)) TrimSplit(y.names) else CapWords(y)
  x.names <- if (!is.null(x.names)) TrimSplit(x.names) else CapWords(x)

  # Create job group
  if (is.null(job.group)) job.group <- list ( c("beta0","zbeta0","sigma","zsigma") ,
                                              c("beta","zbeta")
  )

  # Final name list
  if (x.steps > 1) {
    job.names <- list(list(sprintf("Intercept (block: %s)", seq(x.steps))),
                      list(sprintf("%s (block: %s)", y.names, seq(x.steps)), x.names)
    )
  } else {
    job.names <- list(list("Intercept"), list(y.names,x.names))  
  }

  # Create crosstable for y parameters
  n.data <- data.frame(t(combn(job.names, 2)),n)

  # Paramter(s) of interest
  params <- if(length(params)) TrimSplit(params) else c("beta0", "beta", "sigma", "zbeta0", "zbeta", "zsigma")

  # Create data for Jags
  data.list <- list(
    x = x.matrix,
    y = y.matrix,
    n = n,
    n.x = x.blocks,
    q = x.steps)

  # Create name list
  name.list <- list(
    job.group = job.group,
    job.names = job.names
  )

  # Return data list
  return (list(
    data.list = data.list,
    name.list = name.list,
    params = params,
    n.data = n.data
  ))

}
