library(BayesFactor)
library(BayesVarSel)

# Defaults
no_prior_information = FALSE
predict = FALSE
range_false_predicts = 10
standardize = FALSE
covariate_probabilities = NULL
desired_sparsity = NULL
desired_prior_effect = NULL
train_percent = NULL

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
  
  if (train == TRUE) {
    if (is.null(train_percent) == TRUE){return("Please input a percent to be set aside for training")}
    train_data_length <- round(train_percent*nrow(data), digits = 0)
    train_data_ind <- sample(1:nrow(data), size = train_data_length, replace = FALSE)
    train <- data[train_data_ind]
    test <- data[-train_data_ind]
    data <- train
    y <- y[train_data_ind]
    test_y <- y[-train_data_ind]
  }
  
  ### First layer of Decision Tree (BIC Bayes Factor)
  if (no_prior_information == TRUE){
    BIC_function(data)
    break
  }

  ### Second Layer of Decision Tree ()
  if (is.null(covariate_probabilities) == FALSE){
    for (prior in c("Robust", "gZellner", "Liangetal", "ZellnerSiow", "FLS")){
      # Small p
      if (ncol(data) <= 25){
        paste0("bf_obj", prior) <- Bvs(formula = response ~ ., 
                      data=data.frame(data,response), 
                      prior.betas = prior,
                      prior.models = "User",
                      priorprobs = covariate_probabilities)
        return(paste(prior) + paste0("bf_obj", prior))
        coef <- BMAcoeff(paste0("bf_obj", prior))
        for (col in colnames(data)){
          return(histBMA(coef, covariate = col))
        }
        if (train == TRUE){
          return(predict.Bvs(paste0("bf_obj", prior), test))
        }
      }
      # Large p
      else{
        paste0("bf_obj", prior) <- GibbsBvs(formula = response ~ ., 
                           data=data.frame(data,response), 
                           prior.betas = prior,
                           prior.models = "User",
                           priorprobs = covariate_probabilities)
        return(final_output(paste0("bf_obj", prior)), predict)
      }
    }
  }
  
  ### Third Layer of Decision Tree (BayesFactor)
  if ((desired_sparisty <= 0.4 && desired_prior_effect < 0.5) || 
      (desired_sparsity <= 0.4 && is.null(desired_prior_effect) == TRUE) ||
      (is.null(desired_sparsity) == TRUE && desired_prior_effect < 0.5)){
    return(regressionBF(formula = response~., 
                        data= data.frame(data, reponse),
                        rscaleCont = "medium",
                        whichModels = "all"
                        ))
  }
    if ((desired_sparisty >= 0.6 && desired_prior_effect > 0.5) || 
        (desired_sparsity >= 0.6 && is.null(desired_prior_effect) == TRUE) ||
        (is.null(desired_sparsity) == TRUE && desired_prior_effect > 0.5)){
      return(regressionBF(formula = response~., 
                          data= data.frame(data, reponse),
                          rscaleCont = "medium",
                          whichModels = "all"
      ))
    }
      else {
        return(regressionBF(formula = response~., 
                            data= data.frame(data, response),
                            rscaleCont = "wide",
                            whichModels = "all"
        ))
        
        # can i return two things here?

  }
  
  
}

BIC_function <- function(data){
  full_lm = lm(score ~ 1, intercept_data)  # One mean with gauss residual
  null_lm = lm(score ~ 0, intercept_data)  # Fixed mean at score = 0
  BF_BIC = exp((BIC(null_lm) - BIC(full_lm))/2)  # From BICs to Bayes factor
  BF_BIC  # Show it
  # return BF for all possible models
}


