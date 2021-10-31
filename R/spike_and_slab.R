
## SPIKE AND SLAB ##

# spike_and_slab_helper <- function(expected_sparsity, beta_variances, zellner_g) {
#   if (!is.na(expected_sparsity)) {
#     es <- 1
#     info <- expected_sparsity
#   } else {
#     if (!is.na(beta_variances)) {
#       es <- 2
#       info <- beta_variances
#     } else if (!is.na(zellner_g)){
#       es <- 3
#       info <- zellner_g
#     } else {
#       print ("An expected sparsity level, beta variances, or Zellner-G value is expected. If you do not have these, please try with prior_knowledge set to FALSE")
#     }
#   }
#   return (c(es, info))
# }

spike_and_slab <- function(data, 
                           predictors, 
                           xdim, 
                           response, 
                           expected.r2, 
                           prior.df, 
                           expected_percent_nonzero, 
                           zellner_g, 
                           p, 
                           prior.information.weight, 
                           diagonal.shrinkage,
                           max.flips,
                           prior.inclusion.probabilities,
                           mean.y,
                           sdy,
                           sigma.upper.limit) {
  design <- model.matrix(predictors)
  
  if (!is.na(zellner_g)) {
    if (is.na(object)) {
      if (is.na(xdim) && !is.na(predictors)) {
        xdim = dim(predictors)
      } else if (is.na(xdim) && is.na(predictors)) {
        print("Please enter the expected dimensions of the predictor matrix or the values of the predictors")
        return()
      }
      # calculate the prior
      prior <- ConditionalZellnerPrior(xdim, 
                                       optional.coefficient.estimate = optional.coefficient.estimate,
                                       expected.model.size = expected_percent_nonzero*p,
                                       prior.information.weight = prior.information.weight,
                                       diagonal.shrinkage = diagonal.shrinkage,
                                       max.flips = max.flips,
                                       prior.inclusion.probabilities = prior.inclusion.probabilities) # use if predictor matrix is unknown
      prior
      # fit the model
      fit <- lm.spike(prior)
      fit
    } else {
      # calculate the prior
      prior <- SpikeSlabPrior(x = design,
                              y = response,
                              expected.r2 = expected.r2,
                              prior.df = prior.df,
                              expected.model.size = expected_percent_nonzero*p,
                              prior.information.weight = prior.information.weight,
                              diagonal.shrinkage = 0,
                              optional.coefficient.estimate = coeff_estimate,
                              max.flips = max.flips,
                              mean.y = mean.y,
                              sdy = sdy,
                              prior.inclusion.probabilities = prior.inclusion.probabilities,
                              sigma.upper.limit = sigma.upper.limit)
      prior
      # fit the model
      fit <- lm.spike(prior)
      fit
    }
    
  } else {
    # calculate the prior
    prior <- SpikeSlabPrior(x = design,
                            y = response,
                            expected.r2 = expected.r2,
                            prior.df = prior.df,
                            expected.model.size = expected_percent_nonzero*p,
                            prior.information.weight = prior_weight,
                            diagonal.shrinkage = diagonal_shrinkage,
                            optional.coefficient.estimate = coeff_estimate,
                            max.flips = max_flips,
                            mean.y = mean_y,
                            sdy = sd_y,
                            prior.inclusion.probabilities = prior.inclusion.probabilities,
                            sigma.upper.limit = sigma.upper.limit)
    prior
    # fit the model
    fit <- lm.spike
    fit
  }
  
}



x <- cbind(1, matrix(rnorm(900), ncol = 9))
beta <- rep(0, 10)
beta[1] <- 4
beta[5] <- -8
beta[8] <- 3
y <- rnorm(100, x %*% beta)
## x has 10 columns, including the intercept
prior <- SpikeSlabPrior(x, y,
  expected.model.size = 3,  # expect 3 nonzero predictors
  prior.df = .01,           # weaker prior than the default
  prior.information.weight = .01,
  diagonal.shrinkage = 0,   # use Zellner's prior
  optional.coefficient.estimate = rep(0, 10) # shrink to zero
)

ConditionalZellnerPrior(xdim,
  optional.coefficient.estimate = NULL,
  expected.model.size = 1,
  prior.information.weight = .01,
  diagonal.shrinkage = .5,
  max.flips = -1,
  prior.inclusion.probabilities = NULL)



