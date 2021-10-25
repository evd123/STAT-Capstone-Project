
### UMBRELLA FUNCTION ###

evaluate <- function(p, prior_knowledge, expected_sparsity, beta_variances, zellner_g, response, goal) {
  if (p < 80) {
    # Bayes Factor
    print("Because the number of predictors is relatively low, we recommend using Bayes Factor. Run <>")
  } else {
    if (prior_knowledge) {
      # Spike and Slab
      print ("Because you have prior knowledge on the data (?), we recommend using Spike and Slab. Run <>")
    } else {
      # 
      if (response == "indicator") {
        print ("Because you want an indicator response, we recommend using Spike and Slab. Run <>")
      } else if (response == "continuous") {
        if (goal == "exploratory") {
          print ("Because your goal is data exploration, we recommend using Spike and Slab. Run <>")
        } else if (goal = "prediction") {
          print ("Because your goal is prediction, we recommend using Continuous Shrinkage. Run <>")
        }
      }
    }
  }
}

evaluate <- function(p, prior_knowledge, expected_sparsity, beta_variances, zellner_g, response, goal) {
  if (p < 80) {
    # Bayes Factor
    return (bayes_factor())
  } else {
    if (prior_knowledge) {
      # Definitely Spike and Slab; need to figure out hyperparameters
      es <- spike_and_slab_helper[1]
      info <- spike_and_slab_helper[2]
      return (spike_and_slab(es, info))
    } else {
      # 
      if (response == "indicator") {
        spike_and_slab_helper
      } else if (response == "continuous") {
        if (goal == "exploratory") {
          spike_and_slab_helper
        } else if (goal = "prediction") {
          return (continuous_shrinkage())
        }
      }
    }
  }
}

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

