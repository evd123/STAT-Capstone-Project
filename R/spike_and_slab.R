
## SPIKE AND SLAB ##

spike_and_slab_helper <- function(expected_sparsity, beta_variances, zellner_g) {
  if (!is.na(expected_sparsity)) {
    es <- 1
    info <- expected_sparsity
  } else {
    if (!is.na(beta_variances)) {
      es <- 2
      info <- beta_variances
    } else if (!is.na(zellner_g)){
      es <- 3
      info <- zellner_g
    } else {
      print ("An expected sparsity level, beta variances, or Zellner-G value is expected. If you do not have these, please try with prior_knowledge set to FALSE")
    }
  }
  return (c(es, info))
}

spike_and_slab <- function(hyperparameters, p, ) {
  if (hyperparameters == "zellner g") {
    # calculate the prior
    prior <- ConditionalZellnerPrior()
    # fit the model
    fit <- lm.spike
  } else {
    # calculate the prior
    prior <- SpikeSlabPrior()
    # fit the model
    fit <- lm.spike
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



