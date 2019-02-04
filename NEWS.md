# bfw 0.4.0

#### Critical 

* Fixed an sorting error in `contrasts` function

#### Minor

* Added the ability to use custom save names
* Removed the appended "Plot" in save names for `ParsePlot` function

# bfw 0.3.0.9002

#### Minor

* Fixed factor-to-numeric-bug `metric` function

# bfw 0.3.0.9001

### Feature

* Added `InverseHDI` function to compute inverse cumulative density function of the distribution

#### Minor

* Added option to add results to `ETA` function

# bfw 0.3.0

### Feature

* Added `PlotParam` function to plot density of parameter values (including ROPE)

#### Moderate

* Removed `PlotData` function. All plots are now called from seperate functions:
    + `PlotCirclize` to create a circlize plot
    + `PlotMean` to create a mean plot
    + `PlotNominal` to create a nominal plot
    + `PlotParam` to create a density plot with parameter values

#### Minor

* Small fix in `SumMCMC` function to compute sample sizes 

# bfw 0.2.0.9005

### Feature

* Updated `CFA` function to include correlation matrix
* Added a option to run `PPP` for every kth length of MCMC chains (Default is every 10th)

#### Moderate

* Optimized `RunContrasts` to allow larger MCMC simulations (2nd review)

#### Minor

* Fixed `plot_data` vignette
* Updated `README`
* Fixed title bug in `circlize` plots
* Added `RemoveGarbage` function to clear up working memory
* Added `MultiGrep`  function to use multiple patterns to select an element from a vector
* Fixed bug in `kappa` function
* Fixed bug in `covariate` function
* Fixed inline comment bug in `TidyCode` function
* Added option to define which parameters to use for diagnostics
* Removed (some of the...) unnecessary arguments in `bfw` function
* Added a `apa` PowerPoint template

# bfw 0.2.0.9004

### Feature

* Added `nominal` and `circlize` (using the `circlize` package) plot types
    + `mean` plots are now seperated from main `PlotData` function
    + `ParsePlot` and `PlotData` functions are also seperated

#### Critical 

* Fixed an error in `nominal` function

# bfw 0.2.0.9003

#### Minor

* Fixed variables namnes in `nominal` function when using only 1 variable

# bfw 0.2.0.9003

#### Minor

* Fixed issue of line break after HTML tags when rendering Rmd files.

# bfw 0.2.0.9002

#### Moderate

* Fixed `ParsePlot` to accommodate ggplots.
    + `PlotData` now returns `ggplot2` and not `grDevices::recordPlot()`
    
#### Minor

* Added a second badge, a more informative badge, 'cos why not.
    
# bfw 0.2.0.9001

#### Minor

* Some typos

# bfw 0.2.0.9000

#### Minor

* Added a badge because all the cool kids have them
* Fixed small inconsistencies in ParsePlot parameters (i.e., type png with layout pw).
    + Defaults are now rasterized pdf on a4 layout with 12 pointsize.

# bfw 0.2.0

#### Critical

* Optimized `RunContrasts` to allow larger MCMC simulations

#### Moderate 

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

#### Minor

* Added a small `ETA` function to display running time of functions
* Made feedback from MCMC functions more informative
* Fixed `custom model` in `settings`
* Fixed `job.title` bug in covariate
* Fixed NULL values in `TrimSplit` and `CapWords`
* Replaced robust estimates of R^2 with Spearman
* Moved vignettes from `vignettes` to `doc`

# bfw 0.1.0

#### Moderate

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
    
#### Minor

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