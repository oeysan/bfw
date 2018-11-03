#' @title Cohen's Kappa
#' @description Bayesian alternative to Cohen's kappa
#' @param x predictor variable(s), Default: NULL
#' @param x.names optional names for predictor variable(s), Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param ... further arguments passed to or from other methods
#' @examples
#' # Simulate rater data
#' Rater1 <- c(rep(0,20),rep(1,80))
#' set.seed(100)
#' Rater2 <- c(rbinom(20,1,0.1), rbinom(80,1,0.9))
#' data <- data.frame(Rater1,Rater2)
#'
#' \donttest{
#' mcmc <- bfw(project.data = data,
#'             x = "Rater1,Rater2",
#'             saved.steps = 50000,
#'             jags.model = "kappa",
#'             jags.seed = 100,
#'             silent = TRUE)
#'
#' }
#' # Print frequentist and Bayesian kappa
#' library(psych)
#' psych::cohen.kappa(data)$confid[1,]
#' #  lower     estimate  upper
#' #  0.6137906 0.7593583 0.9049260
#' #' \donttest{ mcmc$summary.MCMC }
#' #             Mean     Median    Mode      ESS   HDIlo    HDIhi    n
#' #  Kappa[1]:  0.739176 0.7472905 0.7634503 50657 0.578132 0.886647 100
#' @seealso
#'  \code{\link[stats]{complete.cases}}
#' @rdname StatsKappa
#' @export
#' @importFrom stats complete.cases
StatsKappa <- function(x = NULL,
                       x.names = NULL,
                       DF,
                       params = NULL,
                       initial.list = list(),
                       ...
) {

  # Fetch x parameters
  x <- TrimSplit(x)

  # Exclude noncomplete observations
  DF <- DF[stats::complete.cases(DF[, x]), x]

  # Create crosstable for x parameters
  n.data  <- as.data.frame(table(DF[, x]))

  # name.contrasts for creating contrasts
  job.names <- paste(x.names,collapse = " vs. ")

  # Select raters
  rater <- as.matrix(DF[, x])

  # Determine which observations are equal across raters
  equal <- apply(rater, 1, function(x) if (length(unique(x)) > 1) 0 else 1)

  # Number of raters (2)
  n.raters <- length(rater[1, ])

  # Number of categories
  n.categories <- length(unique(rater[,1]))

  # Number of observations
  n <- length(rater[, 1])

  # Paramter(s) of interest
  params <- if(length(params)) TrimSplit(params) else c("Kappa")
  
  # Create data for Jags
  data.list <- list(
    rater = rater,
    alpha = rep(1,n.categories),
    equal = equal,
    n.raters = n.raters,
    n.categories = n.categories,
    n = n
  )

  # Create name list
  name.list <- list(
    job.names = job.names
  )

  return ( list (
    params = params,
    data.list = data.list,
    name.list = name.list,
    n.data = n.data,
    initial.list = initial.list
  ))

}
