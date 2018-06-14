#' @title Softmax Regression
#' @description Perform softmax regression (i.e., multinomial logistic regression)
#' @param y criterion variable(s), Default: NULL
#' @param y.names optional names for criterion variable(s), Default: NULL
#' @param x predictor variable(s), Default: NULL
#' @param x.names optional names for predictor variable(s), Default: NULL
#' @param DF data to analyze
#' @param params define parameters to observe, Default: NULL
#' @param job.group for some hierarchical models with several layers of parameter names (e.g., latent and observed parameters), Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param run.robust logical, indicating whether or not robust analysis, Default: FALSE
#' @param ... further arguments passed to or from other methods
#' @examples
#' # Conduct softmax regression on Cats data
#' # Reward is 0 = Food and 1 = Dance
#' \donttest{
#' mcmc <- bfw(project.data = bfw::Cats,
#'             y = "Alignment",
#'             x = "Ratings,Reward",
#'             saved.steps = 50000,
#'             jags.model = "softmax",
#'             jags.seed = 100,
#'             silent = TRUE)
#' }
#' # Conduct binominal generalized linear model
#' model <- glm(Alignment ~ Ratings + Reward, data=bfw::Cats, family = binomial(link="logit"))
#'
#' # Print output from softmax
#' #' \donttest{ mcmc$summary.MCMC }
#' #                                   Mean     Median          Mode   ESS     HDIlo     HDIhi    n
#' #beta0[1]: Intercept: Evil     0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #beta0[2]: Intercept: Good    -7.6900266 -7.6842450 -7.6591980566 17693 -8.471740 -6.917770 2000
#' #beta[1,1]: Evil vs. Ratings   0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #beta[2,1]: Good vs. Ratings   1.2891109  1.2884400  1.2834031862 19614  1.187080  1.387420 2000
#' #beta[1,2]: Evil vs. Reward    0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #beta[2,2]: Good vs. Reward    1.2755419  1.2748600  1.2792090358 20807  0.961217  1.596540 2000
#' #zbeta0[1]: Intercept: Evil    0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #zbeta0[2]: Intercept: Good   -1.0307617 -1.0300500 -1.0241784961 22812 -1.185420 -0.870468 2000
#' #zbeta[1,1]: Evil vs. Ratings  0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #zbeta[2,1]: Good vs. Ratings  2.4755475  2.4742500  2.4645858712 19614  2.279560  2.664290 2000
#' #zbeta[1,2]: Evil vs. Reward   0.0000000  0.0000000 -0.0006069443     0  0.000000  0.000000 2000
#' #zbeta[2,2]: Good vs. Reward   0.5005214  0.5002545  0.5019603414 20807  0.377181  0.626482 2000
#'
#' # Print (truncated) output from GML
#' # Coefficients:
#' #               Estimate   Std. Error z value Pr(>|z|)
#' #(Intercept)     -6.39328    0.27255 -23.457  < 2e-16 ***
#' #Ratings          1.28480    0.05136  25.014  < 2e-16 ***
#' #RewardAffection  1.26975    0.16381   7.751  9.1e-15 ***
#' @seealso
#'  \code{\link[stats]{complete.cases}}
#' @rdname StatsSoftmax
#' @export
#' @importFrom stats complete.cases
StatsSoftmax <- function(y,
                         y.names,
                         x,
                         x.names,
                         DF,
                         params,
                         job.group,
                         initial.list,
                         run.robust,
                         ...
) {

  # Split x variables
  x <- TrimSplit(x)

  # Remove non-complete cases across parameters)
  DF <- DF[stats::complete.cases( DF[, c(y,x)] ), ]

  # Create x and y names
  x.names <- if (length(x.names)) TrimSplit(x.names) else CapWords(x)

  # Create y names, either from specified names or variable names
  y.names <- if (!is.null(y.names)) TrimSplit(y.names) else levels(DF[,y])
  # If y.names and y are of unequal length
  if ( length(y.names) != length(levels(DF[,y])) ) {
    warning("y.names and y.levels have unequal length. Using y.levels.")
    y.names <- levels(DF[,y])
  }

  # Select dichotomous variable as criterion
  y.matrix <- as.numeric(DF[, y])
  # Select continous/dichotomous/dummy variables as predictors
  x.matrix = as.matrix(sapply(DF[, x], as.numeric))

  # Create job group
  if (is.null(job.group)) job.group <- list ( c("beta0","zbeta0") ,
                                              c("beta","zbeta")
  )

  # Final name list
  job.names <- list(list(paste0("Intercept: ",y.names)),
                    list(y.names, x.names)
  )

  # Number of observations
  n <- length(y.matrix)

  # Create crosstable for y parameters
  n.data <- data.frame(t(combn(job.names, 2)),n)

  # Paramter(s) of interest
  params <- if(length(params)) TrimSplit(params) else c("beta0", "beta","zbeta0", "zbeta")
  if (run.robust) params <- c(params,"alpha")

  # Create data for Jags
  data.list <- list(
    x = x.matrix,
    y = y.matrix,
    n.x = dim(x.matrix)[2],
    n = dim(x.matrix)[1],
    q = length(y.names)
  )

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
