#' @title Bernoulli Trials
#' @description Conduct bernoulli trials
#' @param x predictor variable(s), Default: NULL
#' @param x.names optional names for predictor variable(s), Default: NULL
#' @param DF data for analysis
#' @param params define parameters to observe, Default: NULL
#' @param initial.list initial values for analysis, Default: list()
#' @param ... further arguments passed to or from other methods
#' @examples
#' # Create coin toss data: heads = 50 and tails = 50
#' fair.coin<- as.matrix(c(rep("Heads",50),rep("Tails",50)))
#' colnames(fair.coin) <- "X"
#' \donttest{
#' fair.coin <- bfw(project.data = fair.coin,
#'                  x = "X",
#'                  saved.steps = 50000,
#'                  jags.model = "bernoulli",
#'                  jags.seed = 100,
#'                  ROPE = c(0.4,0.6),
#'                  silent = TRUE)
#' }
#' fair.coin.freq <- binom.test( 50000 * 0.5, 50000)
#'
#' # Create coin toss data: heads = 20 and tails = 80
#' biased.coin <- as.matrix(c(rep("Heads",20),rep("Tails",80)))
#' colnames(biased.coin) <- "X"
#' \donttest{
#' biased.coin <- bfw(project.data = biased.coin,
#'                    x = "X",
#'                    saved.steps = 50000,
#'                    jags.model = "bernoulli",
#'                    jags.seed = 101,
#'                    initial.list = list(theta = 0.7),
#'                    ROPE = c(0.4,0.6),
#'                    silent = TRUE)
#' }
#' biased.coin.freq <- binom.test( 50000 * 0.8, 50000)
#'
#' # Print Bayesian and frequentist results of fair coin
#' \donttest{ fair.coin$summary.MCMC[,c(3:6,9:12)] }
#'
#'     # Mode       ESS     HDIlo     HDIhi    ROPElo    ROPEhi    ROPEin         n
#'     # 0.505 50480.000     0.405     0.597     2.070     2.044    95.886   100.00
#'
#' sprintf("Frequentist: %.3f [%.3f , %.3f], p = %.3f" ,
#'         fair.coin.freq$estimate ,
#'         fair.coin.freq$conf.int[1] ,
#'         fair.coin.freq$conf.int[2] ,
#'         fair.coin.freq$p.value)
#'
#'     # [1] "Frequentist: 0.500 [0.496 , 0.504], p = 1.000"
#'
#' # Print Bayesian and frequentist results of biased coin
#' \donttest{ biased.coin$summary.MCMC[,c(3:6,9:12)] }
#'
#'     # Mode       ESS     HDIlo     HDIhi    ROPElo    ROPEhi    ROPEin         n
#'     # 0.803 50000.000     0.715     0.870     0.000    99.996     0.004   100.000
#'
#' sprintf("Frequentist: %.3f [%.3f , %.3f], p = %.3f" ,
#'         biased.coin.freq$estimate ,
#'         biased.coin.freq$conf.int[1] ,
#'         biased.coin.freq$conf.int[2] ,
#'         biased.coin.freq$p.value)
#'
#'     # [1] "Frequentist: 0.800 [0.796 , 0.803], p = 0.000"
#' @seealso
#'  \code{\link[stats]{complete.cases}}
#' @rdname StatsBernoulli
#' @export
#' @importFrom stats complete.cases
StatsBernoulli <- function(x = NULL,
                           x.names = NULL,
                           DF,
                           params = NULL,
                           initial.list = list(),
                           ...
) {

  # Create crosstable for x parameters
  n.data  <- as.data.frame(table(DF[, x]))

  # Fetch x parameter and exclude missing observations
  x <- DF[stats::complete.cases(DF[, x]), x]

  # Final name list
  job.names <- paste(levels(x)[[1]],levels(x)[[2]], sep=" vs. ")

  # Convert x to numeric vector (start at zero)
  x <- as.numeric(as.factor(x)) - 1

  # Number of observations
  n <- length(x)

  # Paramter(s) of interest
  if (is.null(params)) params <-  c("theta")

  # Create data for Jags
  data.list <- list(
    x = x,
    n = n
  )

  # Priors
  if (!length(initial.list)) {
      initial.list <- list(
        theta = 0.5
      )
  }

  # Create name list
  name.list <- list(
    job.names = job.names
  )

  # Return data list
  return ( list (
    params = params,
    data.list = data.list,
    name.list = name.list,
    initial.list = initial.list,
    n.data = n.data
  ))

}
