Predict Metric
================
Øystein Olav Skaar
2018-06-09

## Predict Metric

Enjoy this brief demonstration of the predict metric module

First, we steal Field’s (2017) dancing cat example (please see
[Cats.R](extdata/Data/Cats.R))

``` r
# # Define data
# data <- bfw::Cats
# # Aggregate data
# aggregate.data <- stats::aggregate(list(Ratings = data$Ratings), 
#                                    by=list(Reward = data$Reward , 
#                                            Dance = data$Dance ,
#                                            Alignment = data$Alignment), 
#                                    FUN=function(x) c(Mean = mean(x), SD = sd(x)))
# # Describe data
# describe.data <- psych::describe(data)[,c(2:5,10:12)]
# 
# # Print data
# print(aggregate.data, digits = 3)
# #>      Reward Dance Alignment Ratings.Mean Ratings.SD
# #> 1      Food    No      Evil        5.078      0.991
# #> 2 Affection    No      Evil        1.785      0.602
# #> 3      Food   Yes      Evil        4.887      0.925
# #> 4 Affection   Yes      Evil        1.692      0.604
# #> 5      Food    No      Good        3.789      0.934
# #> 6 Affection    No      Good        5.528      0.857
# #> 7      Food   Yes      Good        3.898      1.097
# #> 8 Affection   Yes      Good        5.734      0.809
# print(describe.data, digts = 3)
# #>               n mean   sd median range  skew kurtosis
# #> Reward*    2000 1.81 0.39   2.00     1 -1.58     0.49
# #> Dance*     2000 1.38 0.49   1.00     1  0.49    -1.76
# #> Alignment* 2000 1.35 0.48   1.00     1  0.63    -1.61
# #> Ratings    2000 3.37 1.92   2.69     6  0.38    -1.40
```

### Next we’ll run the Bayesian model to analyze the cats

\`\`\`

### Next we’ll run the Bayesian model to analyze the cats

``` r
# # Use the three categorical variables and mixed contrast.
# ## ... and just show the most likely parameter estimate and sample size.
# 
# mcmc <- bfw::bfw(project.data = data,
#             y = "Ratings",
#             x = "Reward,Dance,Alignment",
#             saved.steps = 50000,
#             jags.model = "metric",
#             run.contrasts = TRUE,
#             use.contrast = "mixed", 
#             contrasts = "1,2,3",
#             jags.seed = 100,
#             silent = TRUE)
# round(mcmc$summary.MCMC[,c(1,7)],3)      
# #>                                                            Mean    n
# #> m1m2m3[1,1,1]: Food vs. No vs. Evil                       1.786   39
# #> m1m2m3[2,1,1]: Affection vs. No vs. Evil                  5.067 1024
# #> m1m2m3[1,2,1]: Food vs. Yes vs. Evil                      1.693  191
# #> m1m2m3[2,2,1]: Affection vs. Yes vs. Evil                 4.887   45
# #> m1m2m3[1,1,2]: Food vs. No vs. Good                       5.527   61
# #> m1m2m3[2,1,2]: Affection vs. No vs. Good                  3.793  116
# #> m1m2m3[1,2,2]: Food vs. Yes vs. Good                      5.734   89
# #> m1m2m3[2,2,2]: Affection vs. Yes vs. Good                 3.900  435
# #> s1s2s3[1,1,1]: Food vs. No vs. Evil                       0.603   39
# #> s1s2s3[2,1,1]: Affection vs. No vs. Evil                  1.005 1024
# #> s1s2s3[1,2,1]: Food vs. Yes vs. Evil                      0.618  191
# #> s1s2s3[2,2,1]: Affection vs. Yes vs. Evil                 0.928   45
# #> s1s2s3[1,1,2]: Food vs. No vs. Good                       0.863   61
# #> s1s2s3[2,1,2]: Affection vs. No vs. Good                  0.944  116
# #> s1s2s3[1,2,2]: Food vs. Yes vs. Good                      0.810   89
# #> s1s2s3[2,2,2]: Affection vs. Yes vs. Good                 1.101  435
# #> b1[1]: Food                                              -0.363  380
# #> b1[2]: Affection                                          0.363 1620
# #> b2[1]: No                                                -0.005 1240
# #> b2[2]: Yes                                                0.005  760
# #> b3[1]: Evil                                              -0.690 1299
# #> b3[2]: Good                                               0.690  701
# #> b1b2[1,1]: Food vs. No                                   -0.023  100
# #> b1b2[2,1]: Affection vs. No                               0.023 1140
# #> b1b2[1,2]: Food vs. Yes                                   0.023  280
# #> b1b2[2,2]: Affection vs. Yes                             -0.023  480
# #> b1b3[1,1]: Food vs. Evil                                 -1.255  230
# #> b1b3[2,1]: Affection vs. Evil                             1.255 1069
# #> b1b3[1,2]: Food vs. Good                                  1.255  150
# #> b1b3[2,2]: Affection vs. Good                            -1.255  551
# #> b2b3[1,1]: No vs. Evil                                    0.073 1063
# #> b2b3[2,1]: Yes vs. Evil                                  -0.073  236
# #> b2b3[1,2]: No vs. Good                                   -0.073  177
# #> b2b3[2,2]: Yes vs. Good                                   0.073  524
# #> b1b2b3[1,1,1]: Food vs. No vs. Evil                      -1.204   39
# #> b1b2b3[2,1,1]: Affection vs. No vs. Evil                  1.350 1024
# #> b1b2b3[1,2,1]: Food vs. Yes vs. Evil                     -1.307  191
# #> b1b2b3[2,2,1]: Affection vs. Yes vs. Evil                 1.161   45
# #> b1b2b3[1,1,2]: Food vs. No vs. Good                       1.158   61
# #> b1b2b3[2,1,2]: Affection vs. No vs. Good                 -1.304  116
# #> b1b2b3[1,2,2]: Food vs. Yes vs. Good                      1.353   89
# #> b1b2b3[2,2,2]: Affection vs. Yes vs. Good                -1.207  435
# #> m1[1]: Food                                               3.685  380
# #> s1[1]: Food                                               0.724  380
# #> m1[2]: Affection                                          4.412 1620
# #> s1[2]: Affection                                          0.995 1620
# #> m2[1]: No                                                 4.043 1240
# #> s2[1]: No                                                 0.854 1240
# #> m2[2]: Yes                                                4.053  760
# #> s2[2]: Yes                                                0.864  760
# #> m3[1]: Evil                                               3.358 1299
# #> s3[1]: Evil                                               0.789 1299
# #> m3[2]: Good                                               4.738  701
# #> s3[2]: Good                                               0.930  701
# #> m1m2[1,1]: Food vs. No                                    3.656  100
# #> s1s2[1,1]: Food vs. No                                    0.733  100
# #> m1m2[2,1]: Affection vs. No                               4.430 1140
# #> s1s2[2,1]: Affection vs. No                               0.975 1140
# #> m1m2[1,2]: Food vs. Yes                                   3.714  280
# #> s1s2[1,2]: Food vs. Yes                                   0.714  280
# #> m1m2[2,2]: Affection vs. Yes                              4.393  480
# #> s1s2[2,2]: Affection vs. Yes                              1.015  480
# #> m1m3[1,1]: Food vs. Evil                                  1.740  230
# #> s1s3[1,1]: Food vs. Evil                                  0.610  230
# #> m1m3[2,1]: Affection vs. Evil                             4.977 1069
# #> s1s3[2,1]: Affection vs. Evil                             0.967 1069
# #> m1m3[1,2]: Food vs. Good                                  5.631  150
# #> s1s3[1,2]: Food vs. Good                                  0.837  150
# #> m1m3[2,2]: Affection vs. Good                             3.846  551
# #> s1s3[2,2]: Affection vs. Good                             1.023  551
# #> m2m3[1,1]: No vs. Evil                                    3.426 1063
# #> s2s3[1,1]: No vs. Evil                                    0.804 1063
# #> m2m3[2,1]: Yes vs. Evil                                   3.290  236
# #> s2s3[2,1]: Yes vs. Evil                                   0.773  236
# #> m2m3[1,2]: No vs. Good                                    4.660  177
# #> s2s3[1,2]: No vs. Good                                    0.903  177
# #> m2m3[2,2]: Yes vs. Good                                   4.817  524
# #> s2s3[2,2]: Yes vs. Good                                   0.956  524
# #> Beta difference: Food/Affection                          -0.726 2000
# #> Beta difference: No/Yes                                  -0.010 2000
# #> Beta difference: Evil/Good                               -1.380 2000
# #> Beta difference: Food/Affection @ No                     -0.047 1240
# #> Beta difference: Food vs. No/Yes                         -0.047  380
# #> Beta difference: Food/Affection vs. No/Yes                0.000 2000
# #> Beta difference: Affection/Food vs. No/Yes                0.000 2000
# #> Beta difference: Affection vs. No/Yes                     0.047 1620
# #> Beta difference: Food/Affection @ Yes                     0.047  760
# #> Beta difference: Food/Affection @ Evil                   -2.511 1299
# #> Beta difference: Food vs. Evil/Good                      -2.511  380
# #> Beta difference: Food/Affection vs. Evil/Good             0.000 2000
# #> Beta difference: Affection/Food vs. Evil/Good             0.000 2000
# #> Beta difference: Affection vs. Evil/Good                  2.511 1620
# #> Beta difference: Food/Affection @ Good                    2.511  701
# #> Beta difference: No/Yes @ Evil                            0.146 1299
# #> Beta difference: No vs. Evil/Good                         0.146 1240
# #> Beta difference: No/Yes vs. Evil/Good                     0.000 2000
# #> Beta difference: Yes/No vs. Evil/Good                     0.000 2000
# #> Beta difference: Yes vs. Evil/Good                       -0.146  760
# #> Beta difference: No/Yes @ Good                           -0.146  701
# #> Beta difference: Food/Affection @ No @ Evil              -2.555 1063
# #> Beta difference: Food vs. No/Yes @ Evil                   0.102  230
# #> Beta difference: Food/Affection vs. No/Yes @ Evil        -2.365 1299
# #> Beta difference: Food @ No vs. Evil/Good                 -2.362  100
# #> Beta difference: Food/Affection @ No vs. Evil/Good        0.099 1240
# #> Beta difference: Food vs. No/Yes vs. Evil/Good           -2.558  380
# #> Beta difference: Food/Affection vs. No/Yes vs. Evil/Good  0.003 2000
# #> Beta difference: Affection/Food vs. No/Yes @ Evil         2.657 1299
# #> Beta difference: Affection vs. No/Yes @ Evil              0.190 1069
# #> Beta difference: Affection/Food @ No vs. Evil/Good        0.193 1240
# #> Beta difference: Affection @ No vs. Evil/Good             2.654 1140
# #> Beta difference: Affection/Food vs. No/Yes vs. Evil/Good -0.003 2000
# #> Beta difference: Affection vs. No/Yes vs. Evil/Good       2.558 1620
# #> Beta difference: Food/Affection @ Yes @ Evil             -2.467  236
# #> Beta difference: Food vs. Yes/No vs. Evil/Good           -2.464  380
# #> Beta difference: Food/Affection vs. Yes/No vs. Evil/Good -0.003 2000
# #> Beta difference: Food @ Yes vs. Evil/Good                -2.660  280
# #> Beta difference: Food/Affection @ Yes vs. Evil/Good      -0.099  760
# #> Beta difference: Affection/Food vs. Yes/No vs. Evil/Good  0.003 2000
# #> Beta difference: Affection vs. Yes/No vs. Evil/Good       2.464 1620
# #> Beta difference: Affection/Food @ Yes vs. Evil/Good      -0.193  760
# #> Beta difference: Affection @ Yes vs. Evil/Good            2.368  480
# #> Beta difference: Food/Affection @ No @ Good               2.461  177
# #> Beta difference: Food vs. No/Yes @ Good                  -0.196  150
# #> Beta difference: Food/Affection vs. No/Yes @ Good         2.365  701
# #> Beta difference: Affection/Food vs. No/Yes @ Good        -2.657  701
# #> Beta difference: Affection vs. No/Yes @ Good             -0.096  551
# #> Beta difference: Food/Affection @ Yes @ Good              2.561  524
# #> Effect size: Food/Affection                              -0.836 2000
# #> Effect size: No/Yes                                      -0.012 2000
# #> Effect size: Evil/Good                                   -1.602 2000
# #> Effect size: Food/Affection @ No                         -0.898 1240
# #> Effect size: Food vs. No/Yes                             -0.079  380
# #> Effect size: Food/Affection vs. No/Yes                   -0.833 2000
# #> Effect size: Affection/Food vs. No/Yes                    0.840 2000
# #> Effect size: Affection vs. No/Yes                         0.037 1620
# #> Effect size: Food/Affection @ Yes                        -0.775  760
# #> Effect size: Food/Affection @ Evil                       -4.011 1299
# #> Effect size: Food vs. Evil/Good                          -5.316  380
# #> Effect size: Food/Affection vs. Evil/Good                -2.505 2000
# #> Effect size: Affection/Food vs. Evil/Good                -0.724 2000
# #> Effect size: Affection vs. Evil/Good                      1.138 1620
# #> Effect size: Food/Affection @ Good                        1.912  701
# #> Effect size: No/Yes @ Evil                                0.172 1299
# #> Effect size: No vs. Evil/Good                            -1.444 1240
# #> Effect size: No/Yes vs. Evil/Good                        -1.576 2000
# #> Effect size: Yes/No vs. Evil/Good                        -1.630 2000
# #> Effect size: Yes vs. Evil/Good                           -1.757  760
# #> Effect size: No/Yes @ Good                               -0.168  701
# #> Effect size: Food/Affection @ No @ Evil                  -3.982 1063
# #> Effect size: Food vs. No/Yes @ Evil                       0.151  230
# #> Effect size: Food/Affection vs. No/Yes @ Evil            -3.968 1299
# #> Effect size: Food @ No vs. Evil/Good                     -5.035  100
# #> Effect size: Food/Affection @ No vs. Evil/Good           -2.543 1240
# #> Effect size: Food vs. No/Yes vs. Evil/Good               -5.531  380
# #> Effect size: Food/Affection vs. No/Yes vs. Evil/Good     -2.388 2000
# #> Effect size: Affection/Food vs. No/Yes @ Evil             4.064 1299
# #> Effect size: Affection vs. No/Yes @ Evil                  0.186 1069
# #> Effect size: Affection/Food @ No vs. Evil/Good           -0.493 1240
# #> Effect size: Affection @ No vs. Evil/Good                 1.311 1140
# #> Effect size: Affection/Food vs. No/Yes vs. Evil/Good     -0.733 2000
# #> Effect size: Affection vs. No/Yes vs. Evil/Good           1.110 1620
# #> Effect size: Food/Affection @ Yes @ Evil                 -4.054  236
# #> Effect size: Food vs. Yes/No vs. Evil/Good               -5.116  380
# #> Effect size: Food/Affection vs. Yes/No vs. Evil/Good     -2.639 2000
# #> Effect size: Food @ Yes vs. Evil/Good                    -5.610  280
# #> Effect size: Food/Affection @ Yes vs. Evil/Good          -2.477  760
# #> Effect size: Affection/Food vs. Yes/No vs. Evil/Good     -0.715 2000
# #> Effect size: Affection vs. Yes/No vs. Evil/Good           1.171 1620
# #> Effect size: Affection/Food @ Yes vs. Evil/Good          -0.972  760
# #> Effect size: Affection @ Yes vs. Evil/Good                0.971  480
# #> Effect size: Food/Affection @ No @ Good                   1.922  177
# #> Effect size: Food vs. No/Yes @ Good                      -0.247  150
# #> Effect size: Food/Affection vs. No/Yes @ Good             1.648  701
# #> Effect size: Affection/Food vs. No/Yes @ Good            -2.210  701
# #> Effect size: Affection vs. No/Yes @ Good                 -0.104  551
# #> Effect size: Food/Affection @ Yes @ Good                  1.901  524    
```

### Uhm. That’s a lot of obscure output

Let’s try to break it down. For instance, the effect size is an
approximation of Cohen’s *d*. Now, if we take a look at **Effect size:
Food/Affection vs. No/Yes vs. Evil/Good**, it clearly indicate a large,
negative effect of some sort. From the aggregate table at the beginning
of the vignette, we can try to interpret the result.

``` r
# ## Let's print the aggregate table again.
# print(aggregate.data, digits = 3)
# #>      Reward Dance Alignment Ratings.Mean Ratings.SD
# #> 1      Food    No      Evil        5.078      0.991
# #> 2 Affection    No      Evil        1.785      0.602
# #> 3      Food   Yes      Evil        4.887      0.925
# #> 4 Affection   Yes      Evil        1.692      0.604
# #> 5      Food    No      Good        3.789      0.934
# #> 6 Affection    No      Good        5.528      0.857
# #> 7      Food   Yes      Good        3.898      1.097
# #> 8 Affection   Yes      Good        5.734      0.809
```

First, we can see that regardless of whether the evil cats dance or not,
they prefer food (*M* = 4.98) as reward over affection (*M* = 1.73).
Second we can see that good cats prefer affection (*M* = 5.63) over food
(*M* = 2.43). Furthermore, we can also infer that evil cats that dance
(*M* = 2.02) rate their owners about the same as evil cats that do not
dance (*M* = 2.11). Good cats, similarly have fairly equal ratings
regardless of whether they dance (*M* = 2.88) or not (*M* = 2.77).
Finally, evil cats (*M* = 2.07) rate their owners somewhat lower than
good cats (*M* = 2.83), as seen by **Effect size: Evil/Good** = -1.60.
From the results claim that evil cats, in general, rate their owners
higher if they get food rather than affection (*d* = -4.01), and that
the opposite is true for good cats (*d* = -1.91).

**Please note that by conducting mixed contrasts results will include
both between and within contrasts, in addition to any possible
combination (including ones that does not necessarily give any
meaning).**

## References

  - Field, A. (2017). *Discovering statistics using IBM SPSS statistics
    (5th edition).* Thousand Oaks, CA: SAGE Publications.
