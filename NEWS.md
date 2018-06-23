# bfw 0.1.0:9000

## Minor

* Fixed small bug in 'fit data' (names of latent variables if not defined by user)

# bfw 0.1.0

## Moderate

* Migrated from the orphaned `ReporteRs` to `officer` (thanks to Professor Brian Ripley at University of Oxford for notifying me)
    + Added two PowerPoint templates 'legacy' (4:3) and 'widescreen' (16:9)
* The following packages are moved from 'Imports' to 'Suggests'
    + ggplot2
    + lavaan
    + officer
    + psych
    + robust
    + rvg
    + scales
    + truncnorm
    
## Minor

* Modified title of package from 'Computational Modelling' to 'Computational Modeling' to conform with US spelling
* Recoded diagnostics, making the code more efficent.
* Correcting some typos and code aesthetics (e.g., replaced print with cat to display running time of analyses)
* Reviewed 'TrimSplit' function to include removing empty elements from vector
* Removed 'methods' and 'rJava' from imports

# bfw 0.0.1

* Initial launch with the following modules:
    + Bernoulli trials
    + Covariate estimations (including correlation and Cronbach's alpha)
    + Fit data (e.g., SEM, CFA, mediation models)
    + Bayesian equivalent of Cohen's kappa
    + Mean and standard deviation estimations
    + Predict metric values (cf., ANOVA)
    + Predict nominal values (c.f., chi-square test)
    + Simple, multiple and hierarchical regression
    + Softmax regression (i.e., multinomial logistic regression)
* Added a `NEWS.md` file to track changes to the package.
* Added a `TODO.md` file to track future work on the package.
* Added a `README.md` file as an introduction to the package.