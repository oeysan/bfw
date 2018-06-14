
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bfw: Bayesian Framework for Computational Modelling

The purpose of bfw is to establish a framework for conducting Bayesian
analysis, using MCMC and JAGS (Plummer, 2003). The framework provides
several modules to conduct linear and non-linear analyses, and allows
the use of custom functions and complex JAGS models.

Derived from the work the excellent work of Kruschke (2015), the goal of
the framework is to easily estimate parameter values and the stability
of estimates from the highest density interval (HDI), make null value
assessment through region of practical equivalence testing (ROPE) and
conduct diagnostics (e.g., Gelman & Rubin, 1992). Though the initial
version only support plotting mean data (including repeated measures),
future released will support other types of visualizations.

Users are encouraged to apply justified priors by modifying existing
models or adding custom models. After careful considerations the
adjustment of priors are done my modifying the models found in
`extdata/Models`, or by adding new ones.

## List of modules

  - Bernoulli trials
  - Covariate estimations (including correlelation and Cronbach’s alpha)
  - Fit data (e.g., SEM, CFA, mediation models)
  - Bayesian equivalent of Cohen’s kappa
  - Mean and standard deviation estimations
  - Predict metric values (cf., ANOVA)
  - Predict nominal values (c.f., chi-square test)
  - Simple, multiple and hierarchical regression
  - Softmax regression (i.e., multinomial logistic regression)

## Prerequisites

  - JAGS (\>=4.3.0): <http://mcmc-jags.sourceforge.net/>
  - Java JDK (\>=1.4): <https://www.java.com/en/download/manual.jsp>

## Dependencies

Dependencies are automatically installed from CRAN. By default, outdated
dependencies are automatically upgraded.

## Installing

You can install `bfw` from GitHub. If you already have a previous
version of `bfw` installed, using the command below will update to the
latest development version.

### Development. version (GitHub)

``` r
githubinstall::githubinstall("bfw")
```

Please note that stable versions are hosted at CRAN, wheras GitHub
versions are in active development.

### Stable version (CRAN)

``` r
utils::install.packages("bfw")
```

Please report any bugs/issues
[here](https://github.com/oeysan/bfw/issues/)

## Example 1: Normal distributed data

Compute mean and standard deviation estimates.  
Please see manual for more
examples.

``` r
# Apply MASS to create normal distributed data with mean = 0 and standard deviation = 1
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

# Run *t* distribution analysis on data 
mcmc.robust <- bfw(project.data = data,
                   y = "y",
                   saved.steps = 50000,
                   jags.model = "mean",
                   jags.seed = 101,
                   ROPE = c(-0.5,0.5),
                   run.robust = TRUE,
                   silent = TRUE)

# Use psych to describe the normally distributed data
psych::describe(data)
#>    vars   n mean sd median trimmed  mad   min  max range  skew kurtosis
#> X1    1 100    0  1   0.05    0.01 1.11 -1.97 2.11  4.07 -0.05    -0.66
#>     se
#> X1 0.1

# Print summary of normal distribution analysis
## Only the most relevant information is shown here
round(mcmc$summary.MCMC[,c(3:6,9:12)],3)
#>               Mode   ESS  HDIlo HDIhi ROPElo ROPEhi ROPEin   n
#> mu[1]: Y    -0.003 49830 -0.201 0.196      0      0    100 100
#> sigma[1]: Y  0.995 48890  0.869 1.150      0    100      0 100

# Print summary of *t* distribution analysis
round(mcmc.robust$summary.MCMC[,c(3:6,9:12)],3)
#>              Mode   ESS  HDIlo HDIhi ROPElo ROPEhi ROPEin   n
#> mu[1]: Y    0.003 31764 -0.195 0.205      0      0    100 100
#> sigma[1]: Y 0.974 27236  0.841 1.135      0    100      0 100
```

## Example 2: Same data but with outliers

``` r
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

# Ruun *t* distribution analysis on biased data 
biased.mcmc.robust <- bfw(project.data = biased.data,
                   y = "y",
                   saved.steps = 50000,
                   jags.model = "mean",
                   jags.seed = 103,
                   ROPE = c(-0.5,0.5),
                   run.robust =TRUE,
                   silent = TRUE)

# Use psych to describe the biased data
psych::describe(data)
#>    vars   n mean sd median trimmed  mad   min  max range  skew kurtosis
#> X1    1 100    0  1   0.05    0.01 1.11 -1.97 2.11  4.07 -0.05    -0.66
#>     se
#> X1 0.1

# Print summary of normal distribution analysis on biased data
## Only the most relevant information is shown here
round(biased.mcmc$summary.MCMC[,c(3:6,9:12)],3)
#>              Mode   ESS HDIlo HDIhi ROPElo ROPEhi ROPEin   n
#> mu[1]: Y    0.922 50000 0.346 1.497      0  91.95   8.05 110
#> sigma[1]: Y 2.995 49069 2.662 3.476      0 100.00   0.00 110

# # Print summary of *t* distribution analysis on biased data
round(biased.mcmc.robust$summary.MCMC[,c(3:6,9:12)],3)
#>              Mode   ESS  HDIlo HDIhi ROPElo ROPEhi ROPEin   n
#> mu[1]: Y    0.049 31088 -0.167 0.268      0  0.004 99.996 110
#> sigma[1]: Y 0.829 18323  0.641 1.077      0 99.994  0.006 110
```

## Example 3: Using custom function and model

Shamelessy adapted from
[here](http://jamescurran.co.nz/2014/06/bayesian-modelling-of-left-censored-data-using-jags/)
(credits to James Curran).

``` r
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
  return ( list (
    params = "lambda",
    data.list = data.list,
    n.data = as.matrix(n)
  ))
  
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
#>      Mode     HDIlo     HDIhi    ROPElo    ROPEhi    ROPEin         n 
#>     1.033     1.003     1.065     7.246    14.030    78.724 10000.000
```

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details

## Acknowledgments

  - John Kruschke for his workshop at ICPSR2016, opening my eyes to
    Bayesian statistics
  - Martyn Plummer for his work on JAGS

## References

  - Gelman, A., & Rubin, D. B. (1992). Inference from Iterative
    Simulation Using Multiple Sequences. *Statistical Science*, 7(4),
    457-472. <https://doi.org/10.1214/ss/1177011136>
  - Kruschke, J. K. (2015). *Doing Bayesian data analysis: a tutorial
    with R, JAGS, and Stan*. Academic Press: Boston.
  - Plummer, M. (2003). JAGS A program for analysis of Bayesian
    graphical models using Gibbs sampling (Version 4.3.0). Retrieved
    from <http://mcmc-jags.sourceforge.net/>
