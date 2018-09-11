# bfw 0.2.0

## Critical

* Optimized `RunContrasts` to allow larger MCMC simulations

## Moderate 

* Optimized `ParsePlot` to handle large amounts of plots
* Added `png` package to `Suggests` to handle rasterized graphics for pdf/ps.
* Created a small function called `TidyCode` to format messy code.
* Modified `nominal` and `metric` functions to use a single flexible jags model for each function respectively 
* Added an extended `covariate function` to include y and x variables.
* Redefined fit `data models`
    + Created a separate model for confirmatory factor analysis using Wishart distribution
    + Redefined structural equation models, making the models more efficient 
* Added options to have more control over JAGS simulations
    + `jags.method`, specify method for JAGS (e.g., parallel or simple)
    + `jags.chains`, specify specify number of chains for JAGS
    + `adapt.steps`, the number of adaptive iterations to use at the start of each simulation
    + `burnin.steps`, the number of burnin iterations, NOT including the adaptive iterations to use for the simulation.

## Minor
* Added a small `ETA` function to display running time of functions
* Made feedback from MCMC functions more informative
* Fixed `custom model` in `settings`
* Fixed `job.title` bug in covariate
* Fixed NULL values in `TrimSplit` and `CapWords`
* Replaced robust estimates of R^2 with Spearman
* Moved vignettes from `vignettes` to `doc`

# bfw 0.1.0

## Moderate

* Migrated from the orphaned `ReporteRs` to `officer` (thanks to Professor Brian Ripley at University of Oxford for notifying me)
    + Added two PowerPoint templates `legacy` (4:3) and `widescreen` (16:9)
* The following packages are moved from `Imports` to `Suggests`
    + ggplot2
    + lavaan
    + officer
    + psych
    + robust
    + rvg
    + scales
    + truncnorm
    
## Minor

* Modified title of package from `Computational Modelling` to `Computational Modeling` to conform with US spelling
* Recoded diagnostics, making the code more efficent.
* Correcting some typos and code aesthetics (e.g., replaced print with cat to display running time of analyses)
* Reviewed `TrimSplit` function to include removing empty elements from vector
* Removed `methods` and `rJava` from imports

# bfw 0.0.1

* Initial launch with the following modules:
    + Bernoulli trials
    + Covariate estimations (including correlation and Cronbach`s alpha)
    + Fit data (e.g., SEM, CFA, mediation models)
    + Bayesian equivalent of Cohen`s kappa
    + Mean and standard deviation estimations
    + Predict metric values (cf., ANOVA)
    + Predict nominal values (c.f., chi-square test)
    + Simple, multiple and hierarchical regression
    + Softmax regression (i.e., multinomial logistic regression)
* Added a `NEWS.md` file to track changes to the package.
* Added a `TODO.md` file to track future work on the package.
* Added a `README.md` file as an introduction to the package.