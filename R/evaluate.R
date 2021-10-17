
### UMBRELLA FUNCTION ###

evaluate <- function(p, prior_knowledge, expected_sparsity, beta_variances, zellner_g, response, goal) {
  if (p < 300) {
    # Bayes Factor
    return (bayes_factor())
  } else {
    if (prior_knowledge) {
      # Definitely Spike and Slab; need to figure out hyperparameters
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
      return (spike_and_slab(es, info))
    } else {
      # 
      if (response == "indicator") {
        
      } else if (response == "continuous") {
        if (goal == "exploratory") {
          
        } else if (goal = "prediction") {
          return (continuous_shrinkage())
        }
      }
    }
  }
}

