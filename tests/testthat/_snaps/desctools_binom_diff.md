# desctools_binom produces correct output

    Code
      res
    Output
           est    lwr.ci   upr.ci
      [1,]   0 -0.400076 0.400076

# desctools_binom produces correct output for all methods

    Code
      res
    Output
                   ac       wald     waldcc      score    scorecc         mn
      [1,]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
      [2,] -0.3357543 -0.3678005 -0.4678005 -0.3262989 -0.3806657 -0.3530546
      [3,]  0.3357543  0.3678005  0.4678005  0.3262989  0.3806657  0.3530546
                  mee        blj         ha        hal         jp
      [1,]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
      [2,] -0.3451998 -0.3678005 -0.4376957 -0.3451925 -0.3451925
      [3,]  0.3451998  0.3678005  0.4376957  0.3451925  0.3451925

# desctools_binomci produces correct output with default settings

    Code
      res
    Output
           est   lwr.ci   upr.ci
      [1,] 0.5 0.299298 0.700702

# desctools_binomci produces correct output with custom settings

    Code
      res
    Output
           est    lwr.ci upr.ci
      [1,] 0.5 0.3596101      1

# desctools_binomci produces correct output for all methods

    Code
      res
    Output
              wilson      wald    waldcc agresti-coull  jeffreys modified wilson
      [1,] 0.5000000 0.5000000 0.5000000     0.5000000 0.5000000       0.5000000
      [2,] 0.3274038 0.3160998 0.2910998     0.3274038 0.3242344       0.3274038
      [3,] 0.6725962 0.6839002 0.7089002     0.6725962 0.6757656       0.6725962
            wilsoncc modified jeffreys clopper-pearson   arcsine     logit   witting
      [1,] 0.5000000         0.5000000       0.5000000 0.5000000 0.5000000 0.5000000
      [2,] 0.3055729         0.3242344       0.3019539 0.3202181 0.3239669 0.3488144
      [3,] 0.6944271         0.6757656       0.6980461 0.6797819 0.6760331 0.6316294
               pratt      midp       lik    blaker
      [1,] 0.5000000 0.5000000 0.5000000 0.5000000
      [2,] 0.3347123 0.3200831 0.3221477 0.3217139
      [3,] 0.6980960 0.6799169 0.6778523 0.6782861

