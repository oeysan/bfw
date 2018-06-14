Regression
================
Øystein Olav Skaar
2018-06-09

## Regression

Enjoy this brief demonstration of the regression
module

### First we simulate some data

``` r
# # Create normal distributed data with mean = 0 and standard deviation = 1
# ## r = 0.5
# data <- MASS::mvrnorm(n=100, 
#                       mu=c(0, 0), 
#                       Sigma=matrix(c(1, 0.5, 0.5, 1), 2), 
#                       empirical=TRUE)
# # Add names
# colnames(data) <- c("x","y")
```

### Check the correlation and regression results using frequentist methods

``` r
# # Correlation
# stats::cor(data)[2]
# #> [1] 0.5
# 
# # Regression
# summary(stats::lm(y ~ x, data=data.frame(data)))
# #> 
# #> Call:
# #> stats::lm(formula = y ~ x, data = data.frame(data))
# #> 
# #> Residuals:
# #>      Min       1Q   Median       3Q      Max 
# #> -3.13163 -0.54590 -0.05902  0.58962  2.13402 
# #> 
# #> Coefficients:
# #>              Estimate Std. Error t value Pr(>|t|)    
# #> (Intercept) 8.327e-17  8.704e-02   0.000        1    
# #> x           5.000e-01  8.748e-02   5.715 1.18e-07 ***
# #> ---
# #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# #> 
# #> Residual standard error: 0.8704 on 98 degrees of freedom
# #> Multiple R-squared:   0.25,  Adjusted R-squared:  0.2423 
# #> F-statistic: 32.67 on 1 and 98 DF,  p-value: 1.18e-07
```

### Then the regression results using the Bayesian model

``` r
# mcmc <- bfw::bfw(project.data = data,
#             y = "y",
#             x = "x",
#             saved.steps = 50000,
#             jags.model = "regression",
#             jags.seed = 100,
#             silent = TRUE)
# # Print the results            
# round(mcmc$summary.MCMC[,3:7],3)
# #>                        Mode   ESS  HDIlo HDIhi   n
# #> beta0[1]: Intercept  -0.008 50000 -0.172 0.173 100
# #> beta[1]: Y vs. X      0.492 51970  0.330 0.674 100
# #> sigma[1]: Y vs. X     0.863 28840  0.760 1.005 100
# #> zbeta0[1]: Intercept -0.008 50000 -0.172 0.173 100
# #> zbeta[1]: Y vs. X     0.492 51970  0.330 0.674 100
# #> zsigma[1]: Y vs. X    0.863 28840  0.760 1.005 100
# #> R^2 (block: 1)        0.246 51970  0.165 0.337 100
```

### Now we add some noise

``` r
# # Create noise with mean = 10 / -10 and sd = 1
# ## r = -1.0
# noise <- MASS::mvrnorm(n=2, 
#                        mu=c(10, -10), 
#                        Sigma=matrix(c(1, -1, -1, 1), 2), 
#                        empirical=TRUE)
# # Combine data
# biased.data <- rbind(data,noise)
```

### Repeat

``` r
# # Correlation
# stats::cor(biased.data)[2]
# #> [1] -0.4984437
# 
# # Regression
# summary(stats::lm(y ~ x, data=data.frame(biased.data)))
# #> 
# #> Call:
# #> stats::lm(formula = y ~ x, data = data.frame(biased.data))
# #> 
# #> Residuals:
# #>     Min      1Q  Median      3Q     Max 
# #> -5.2719 -0.8431  0.1007  0.9851  3.0101 
# #> 
# #> Coefficients:
# #>             Estimate Std. Error t value Pr(>|t|)    
# #> (Intercept) -0.09834    0.14868  -0.661     0.51    
# #> x           -0.49844    0.08669  -5.750 9.75e-08 ***
# #> ---
# #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# #> 
# #> Residual standard error: 1.492 on 100 degrees of freedom
# #> Multiple R-squared:  0.2484, Adjusted R-squared:  0.2409 
# #> F-statistic: 33.06 on 1 and 100 DF,  p-value: 9.754e-08
```

### Finally, using Bayesian model with robust estimates

``` r
# mcmc.robust <- bfw::bfw(project.data = biased.data,
#             y = "y",
#             x = "x",
#             saved.steps = 50000,
#             jags.model = "regression",
#             jags.seed = 100,
#             run.robust = TRUE,
#             silent = TRUE)
# # Print the results            
# round(mcmc.robust$summary.MCMC[,3:7],3)
# #>                        Mode   ESS  HDIlo HDIhi   n
# #> beta0[1]: Intercept  -0.026 29844 -0.204 0.141 102
# #> beta[1]: Y vs. X      0.430 29549  0.265 0.604 102
# #> sigma[1]: Y vs. X     0.671 16716  0.530 0.842 102
# #> zbeta0[1]: Intercept  0.138 28442  0.042 0.244 102
# #> zbeta[1]: Y vs. X     0.430 29549  0.265 0.604 102
# #> zsigma[1]: Y vs. X    0.392 16716  0.310 0.492 102
# #> R^2 (block: 1)        0.236 29549  0.145 0.331 102
```
