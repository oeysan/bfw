<!-- README.md is generated from README.Rmd. Please edit that file -->
*bfw*: Bayesian Framework for Computational Modeling
====================================================

<p align="center">
<a href="man/figures/logo.png" id="logo" title="Logo"><img src="man/figures/logo.png" width="250px" alt="Logo" /></a>
<a href="man/figures/logo2.png" id="logo2" title="Logo II"><img src="man/figures/logo2.png" width="250px" alt="Logo II" /></a>
</p>
<p align="center">
<a href="NEWS.md" id="news" title="News"><img src="https://img.shields.io/badge/News-2018.09.18 @ 19:35:09-purple.svg" alt="News" /></a>
<a href="https://CRAN.R-project.org/package=bfw" id="cran" title="CRAN Version"><img src="https://www.r-pkg.org/badges/version/bfw" alt="CRAN Version" /></a>
<a href="https://github.com/oeysan/bfw" id="github" title="GitHub Version"><img src="https://img.shields.io/badge/GitHub-0.2.0.9004-red.svg?style=flat-square" alt="GitHub Version" /></a>
<br/>
<a href="LICENSE.md" id="license" title="License"><img src="https://img.shields.io/badge/Licence-MIT-blue.svg" alt="License" /></a>
<a href="https://travis-ci.org/oeysan/bfw" id="travis" title="Build Status"><img src="https://travis-ci.org/oeysan/bfw.svg?branch=master" alt="Build Status" /></a>
</p>

What is *bfw*?
--------------

The purpose of *`bfw`* is to establish a framework for conducting
Bayesian analysis in [R](https://www.r-project.org/), using
[MCMC](https://link.springer.com/article/10.3758/s13423-016-1015-8) and
[JAGS](http://mcmc-jags.sourceforge.net/) (Plummer, 2003). The framework
provides several modules to conduct linear and non-linear (hierarchical)
analyses, and allows the use of custom functions and complex JAGS
models.

Derived from the excellent work of Kruschke (2015), the goal of the
framework is to easily estimate parameter values and the stability of
estimates from the *highest density interval* (HDI), make null value
assessment through *region of practical equivalence testing* (ROPE) and
conduct convergence diagnostics (e.g., Gelman & Rubin, 1992). Though the
initial version only support plotting mean data (including repeated
measures), future releases will support other types of visualizations.

Users are encouraged to apply justified priors by modifying existing
JAGS models found in `extdata/models` or by adding custom models.
Similarly, one might modify models to conduct posterior predictive
checks (see Kruschke, 2013). The purpose of the framework is not to
provide generic modules suitable for all circumstances, but rather act
as a platform for modifying or developing models for a given project.

List of current modules
-----------------------

-   Bernoulli trials
-   Covariate estimations (including correlation and Cronbach's alpha)
-   Fit observed and latent data (e.g., SEM, CFA, mediation models)
-   Bayesian equivalent of Cohen's kappa
-   Mean and standard deviation estimations
-   Predict metric values (cf., ANOVA)
-   Predict nominal values (cf., chi-square test)
-   Simple, multiple and hierarchical regression
-   Softmax regression (i.e., multinomial logistic regression)

Prerequisites
-------------

-   JAGS (&gt;=4.3.0): <http://mcmc-jags.sourceforge.net/>
-   Java JDK (&gt;=1.4): <https://www.java.com/en/download/manual.jsp>

Dependencies
------------

Dependencies are automatically installed from CRAN. By default, outdated
dependencies are automatically upgraded.

Installing
----------

You can install *`bfw`* from GitHub. If you already have a previous
version of *`bfw`* installed, using the command below will update to the
latest development version.

#### Development version ([GitHub](https://github.com/oeysan/bfw/))

    devtools::install_github("oeysan/bfw")

Please note that stable versions are hosted at CRAN, whereas GitHub
versions are in active development.

#### Stable version ([CRAN](https://CRAN.R-project.org/package=bfw))

    utils::install.packages("bfw")

#### Issues

Please report any bugs/issues
[here](https://github.com/oeysan/bfw/issues/)

Example 1: Normal distributed data
----------------------------------

Compute mean and standard deviation estimates.  
Please see manual for more examples.

    # Apply MASS to create normal distributed data with mean = 0 and standard deviation = 1
    set.seed(99)
    data <- data.frame(y =
                         MASS::mvrnorm(n=100,
                                       mu=0,
                                       Sigma=matrix(1, 1),
                                       empirical=TRUE) )

    # Run normal distribution analysis on data
    ## Use 50 000 iterations, set seed for replication.
    ### Null value assessment is set at ROPE = -0.5 - 0.5 for mean
    mcmc <- bfw(project.data = data,
                y = "y",
                saved.steps = 50000,
                jags.model = "mean",
                jags.seed = 100,
                ROPE = c(-0.5,0.5),
                silent = TRUE)

    # Run t-distribution analysis on data
    mcmc.robust <- bfw(project.data = data,
                       y = "y",
                       saved.steps = 50000,
                       jags.model = "mean",
                       jags.seed = 101,
                       ROPE = c(-0.5,0.5),
                       run.robust = TRUE,
                       silent = TRUE)

    # Use psych to describe the normally distributed data
    psych::describe(data)[,c(2:12)]
    #>      n mean sd median trimmed  mad   min  max range  skew kurtosis
    #> X1 100    0  1   0.17    0.04 0.81 -3.26 2.19  5.45 -0.57     0.53

    # Print summary of normal distribution analysis
    ## Only the most relevant information is shown here
    round(mcmc$summary.MCMC[,c(3:6,9:12)],3)
    #>               Mode   ESS  HDIlo HDIhi ROPElo ROPEhi ROPEin   n
    #> mu[1]: Y    -0.003 49830 -0.201 0.196      0      0    100 100
    #> sigma[1]: Y  0.995 48890  0.869 1.150      0    100      0 100

    # Print summary of t-distribution analysis
    round(mcmc.robust$summary.MCMC[,c(3:6,9:12)],3)
    #>              Mode   ESS  HDIlo HDIhi ROPElo ROPEhi ROPEin   n
    #> mu[1]: Y    0.027 25887 -0.167 0.229      0      0    100 100
    #> sigma[1]: Y 0.933 10275  0.749 1.115      0    100      0 100

Example 2: Same data but with outliers
--------------------------------------

    # Add 10 outliers, each with a value of 10.
    biased.data <- rbind(data,data.frame(y = rep(10,10)))

    # Run normal distribution analyis on biased data
    biased.mcmc <- bfw(project.data = biased.data,
                       y = "y",
                       saved.steps = 50000,
                       jags.model = "mean",
                       jags.seed = 102,
                       ROPE = c(-0.5,0.5),
                       silent = TRUE)

    # Run t-distribution analysis on biased data
    biased.mcmc.robust <- bfw(project.data = biased.data,
                              y = "y",
                              saved.steps = 50000,
                              jags.model = "mean",
                              jags.seed = 103,
                              ROPE = c(-0.5,0.5),
                              run.robust =TRUE,
                              silent = TRUE)

    # Use psych to describe the biased data
    psych::describe(biased.data)[,c(2:12)]
    #>      n mean   sd median trimmed  mad   min max range skew kurtosis
    #> X1 110 0.91 3.04   0.25    0.21 0.92 -3.26  10  13.3  2.3     4.38

    # Print summary of normal distribution analysis on biased data
    ## Only the most relevant information is shown here
    round(biased.mcmc$summary.MCMC[,c(3:6,9:12)],3)
    #>              Mode   ESS HDIlo HDIhi ROPElo ROPEhi ROPEin   n
    #> mu[1]: Y    0.922 50000 0.346  1.50      0     92   8.05 110
    #> sigma[1]: Y 2.995 49069 2.662  3.48      0    100   0.00 110

    # # Print summary of t-distribution analysis on biased data
    round(biased.mcmc.robust$summary.MCMC[,c(3:6,9:12)],3)
    #>              Mode   ESS  HDIlo HDIhi ROPElo ROPEhi  ROPEin   n
    #> mu[1]: Y    0.168 29405 -0.015 0.355      0  0.008  99.992 110
    #> sigma[1]: Y 0.679 17597  0.512 0.903      0 99.128   0.872 110

Example 3: Custom function and model
------------------------------------

Shamelessly adapted from
[here](http://jamescurran.co.nz/2014/06/bayesian-modelling-of-left-censored-data-using-jags/)
(credits to James Curran).

    # Create a function for left-censored data
    custom.function <- function(DF) {

      x <- as.vector(unlist(DF))
      x[x < log(29)] = NA
      n <- length(x)
      LOD <- rep(log(29), n)
      is.above.LOD <- ifelse(!is.na(x), 1, 0)
      lambda <- 0.5
      initial.x = rep(NA, length(x))
      n.missing <- sum(!is.above.LOD)
      initial.x[!is.above.LOD] <- runif(n.missing, 0, log(29))

      initial.list <- list(lambda = lambda,
                           x = initial.x
      )

      data.list <- list(n = n,
                        x = x,
                        LOD = LOD,
                        is.above.LOD = is.above.LOD
      )

      # Return data list
      return (
        list (
          params = "lambda",
          initial.list = initial.list,
          data.list = data.list,
          n.data = as.matrix(n)
          )
        )
    }

    # Create a model
    custom.model = "
      model{
        for(i in 1:n){
          is.above.LOD[i] ~ dinterval(x[i], LOD[i])
          x[i] ~ dexp(lambda)
        }
        lambda ~ dgamma(0.001, 0.001)
      }
    "

    # Simulate some data
    set.seed(35202)
    project.data <- as.matrix(rexp(10000, rate = 1.05))

    # Run analysis
    custom.mcmc <- bfw(project.data = project.data,
                       custom.function = custom.function,
                       custom.model = custom.model,
                       saved.steps = 50000,
                       jags.seed = 100,
                       ROPE = c(1.01,1.05),
                       silent = TRUE)

    # Print analysis
    round(custom.mcmc$summary.MCMC[,c(3,5,6,9:12)],3)
    #>     Mode    HDIlo    HDIhi   ROPElo   ROPEhi   ROPEin        n 
    #>     1.03     1.00     1.06     7.64    14.33    78.03 10000.00

The cost of conducting robust estimates
---------------------------------------

    # Running time for normal distribution analyis
    biased.mcmc$run.time[2] - biased.mcmc$run.time[1]
    #> Time difference of 9.83 secs

    # Running time for t-distribution analysis
    biased.mcmc.robust$run.time[2] - biased.mcmc.robust$run.time[1]
    #> Time difference of 55.6 secs

License
-------

This project is licensed under the MIT License - see
[LICENSE.md](LICENSE.md) for details

Acknowledgments
---------------

-   John Kruschke for his overall amazing work, and especially for his
    workshop at ICPSR 2016. It opened my eyes to Bayesian statistics.
-   Martyn Plummer for his work on JAGS. Cheers!

References
----------

-   Gelman, A., & Rubin, D. B. (1992). Inference from Iterative
    Simulation Using Multiple Sequences. *Statistical Science*, *7*(4),
    457-472. <https://doi.org/10.1214/ss/1177011136>
-   Kruschke, J. K. (2013). Posterior predictive checks can and should
    be Bayesian: Comment on Gelman and Shalizi, 'Philosophy and the
    practice of Bayesian statistics'. *British Journal of Mathematical
    and Statistical Psychology*, *66*(1), 4556.
    <https://doi.org/10.1111/j.2044-8317.2012.02063.x>
-   Kruschke, J. K. (2015). *Doing Bayesian data analysis: a tutorial
    with R, JAGS, and Stan*. Academic Press: Boston.
-   Plummer, M. (2003). JAGS A program for analysis of Bayesian
    graphical models using Gibbs sampling (Version 4.3.0). Retrieved
    from <http://mcmc-jags.sourceforge.net/>
