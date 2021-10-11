library(BayesFactor)
library(BayesVarSel)

# Defaults
no_prior_information = FALSE
predict = FALSE
range_false_predicts = 10
standardize = FALSE


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
}

BIC_function <- function(data){
  
}