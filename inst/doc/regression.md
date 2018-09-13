Regression
================
Øystein Olav Skaar
2018-09-13

## Regression

Enjoy this brief demonstration of the regression
module

### First we simulate some data

``` r
# Create normal distributed data with mean = 0 and standard deviation = 1
## r = 0.5
data <- MASS::mvrnorm(n=100,
                      mu=c(0, 0),
                      Sigma=matrix(c(1, 0.5, 0.5, 1), 2),
                      empirical=TRUE)
# Add names
colnames(data) <- c("x","y")
```

### Check the correlation and regression results using frequentist methods

``` r
# Correlation
stats::cor(data)[2]
#> [1] 0.5

# Regression
summary(stats::lm(y ~ x, data=data.frame(data)))
#> 
#> Call:
#> stats::lm(formula = y ~ x, data = data.frame(data))
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.1645 -0.6169 -0.0112  0.6460  2.4460 
#> 
#> Coefficients:
#>                           Estimate             Std. Error t value
#> (Intercept) -0.0000000000000000139  0.0870432686210995676    0.00
#> x            0.4999999999999996114  0.0874817765279706366    5.72
#>               Pr(>|t|)    
#> (Intercept)          1    
#> x           0.00000012 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.87 on 98 degrees of freedom
#> Multiple R-squared:  0.25,   Adjusted R-squared:  0.242 
#> F-statistic: 32.7 on 1 and 98 DF,  p-value: 0.000000118
```

### Then the regression results using the Bayesian model

``` r
mcmc <- bfw::bfw(project.data = data,
            y = "y",
            x = "x",
            saved.steps = 50000,
            jags.model = "regression",
            jags.seed = 100,
            silent = TRUE)
# Print the results            
round(mcmc$summary.MCMC[,3:7],3)
#>                        Mode   ESS  HDIlo HDIhi   n
#> beta0[1]: Intercept  -0.008 50000 -0.172 0.173 100
#> beta[1]: Y vs. X      0.492 51970  0.330 0.674 100
#> sigma[1]: Y vs. X     0.863 28840  0.760 1.005 100
#> zbeta0[1]: Intercept -0.008 50000 -0.172 0.173 100
#> zbeta[1]: Y vs. X     0.492 51970  0.330 0.674 100
#> zsigma[1]: Y vs. X    0.863 28840  0.760 1.005 100
#> R^2 (block: 1)        0.246 51970  0.165 0.337 100
```

### Now we add some noise

``` r
# Create noise with mean = 10 / -10 and sd = 1
## r = -1.0
noise <- MASS::mvrnorm(n=2,
                       mu=c(10, -10),
                       Sigma=matrix(c(1, -1, -1, 1), 2),
                       empirical=TRUE)
# Combine data
biased.data <- rbind(data,noise)
```

### Repeat

``` r
# Correlation
stats::cor(biased.data)[2]
#> [1] -0.498

# Regression
summary(stats::lm(y ~ x, data=data.frame(biased.data)))
#> 
#> Call:
#> stats::lm(formula = y ~ x, data = data.frame(biased.data))
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -5.272 -0.921  0.240  1.014  3.737 
#> 
#> Coefficients:
#>             Estimate Std. Error t value    Pr(>|t|)    
#> (Intercept)  -0.0983     0.1487   -0.66        0.51    
#> x            -0.4984     0.0867   -5.75 0.000000098 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.49 on 100 degrees of freedom
#> Multiple R-squared:  0.248,  Adjusted R-squared:  0.241 
#> F-statistic: 33.1 on 1 and 100 DF,  p-value: 0.0000000975
```

### Finally, using Bayesian model with robust estimates

``` r
mcmc.robust <- bfw::bfw(project.data = biased.data,
            y = "y",
            x = "x",
            saved.steps = 50000,
            jags.model = "regression",
            jags.seed = 100,
            run.robust = TRUE,
            silent = TRUE)
# Print the results            
round(mcmc.robust$summary.MCMC[,3:7],3)
#>                        Mode   ESS  HDIlo HDIhi   n
#> beta0[1]: Intercept  -0.026 29844 -0.204 0.141 102
#> beta[1]: Y vs. X      0.430 29549  0.265 0.604 102
#> sigma[1]: Y vs. X     0.671 16716  0.530 0.842 102
#> zbeta0[1]: Intercept  0.138 28442  0.042 0.244 102
#> zbeta[1]: Y vs. X     0.430 29549  0.265 0.604 102
#> zsigma[1]: Y vs. X    0.392 16716  0.310 0.492 102
#> R^2 (block: 1)        0.236 29549  0.145 0.331 102
```