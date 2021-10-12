library(BayesFactor)
library(BayesVarSel)

# Defaults
no_prior_information = FALSE
predict = FALSE
range_false_predicts = 10
standardize = FALSE
covariate_probabilities = NULL


bayes_factor <- function(data, response, desired_sparsity, 
                         predict, range_false_predicts, standardize,
                         train, train_percent, desired_prior_effect,
                         covariate_probabilities, no_prior_information){
  ### CHECKS ###
  if (nrow(data) != length(response)){
    return("Error! Please ensure that the number of elements in the response vector is the same
           as the number of rows in the data data.frame.")
  }
  
  # Question for Emma: Will the return end the function or do I need a break above?
  
  if (standardize == TRUE){
    for (predictor in 1:ncol(data)){
      data[, predictor] <- (data[, predictor] - mean(data[, predictor]))/sd(data[, predictor])
    }
  }
  
  ### First layer of Decision Tree (BIC Bayes Factor)
  if (no_prior_information == TRUE){
    BIC_function(data)
    break
  }
  
  ### Second Layer of Decision Tree ()
  if (is.null(covariate_probabilities) == FALSE){
    if (ncol(data) <= 25){
      bf_obj <- Bvs(formula = response ~ ., 
          data=data.frame(data,response), 
          prior.betas = "Robust",
          prior.models = "User",
          priorprobs = covariate_probabilities)
    }
    else{
      bf_obj <- Bvs(formula = response ~ ., 
          data=data.frame(data,response), 
          prior.betas = "Robust",
          prior.models = "User",
          priorprobs = covariate_probabilities)
    }
  }
}

BIC_function <- function(data){
  full_lm = lm(score ~ 1, intercept_data)  # One mean with gauss residual
  null_lm = lm(score ~ 0, intercept_data)  # Fixed mean at score = 0
  BF_BIC = exp((BIC(null_lm) - BIC(full_lm))/2)  # From BICs to Bayes factor
  BF_BIC  # Show it
  # return BF for all possible models
}