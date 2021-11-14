## Continuous Shrinkage Prior ##
library(bayesreg)
library(horseshoe)
library(Hmisc)

continuous_shrinkage <- function(
                  data, response, no_prior_information = FALSE,
                  desired_sparsity = NULL, desired_prior_effect = NULL,
                  covariate_probabilities = NULL,
                  predict = FALSE, range_false_predicts = 10, 
                  standardize = FALSE,
                  train = FALSE, train_percent=NULL,
                  type = "horseshoe") {
  if (type %in% list(c("lasso", "horseshoe", "ridge"))){
    return("Error: Please input a valid continous shrinkage prior type!")
  }
  
  if (type == "horseshoe"){
    results <- horseshoe(response, data.matrix(data), method.tau = "truncatedCauchy")
    #plot(response, (data.matrix(data))%*%results$BetaHat, col = c(rep("black", 80), rep("blue", 20)))
    #xYplot(Cbind(results$BetaHat, results$LeftCI, results$RightCI) ~ 1:100)
    #return(class(results$BetaHat))
    #return(results$LeftCI)
    res <- cbind(results$BetaHat, results$LeftCI, results$RightCI)
    colnames(res) <- c("Posterior Mean", "95% CI Lower Bound", "95% CI Upper Bound")
    rownames(res) <- colnames(data)
    return(res)
  }
  else {
    results <- bayesreg(response ~., data.frame(data, response), prior = type)
    #return(results$mu.beta)
    return(summary(results))
    #predict(results)
  }
}


n <- 20
column_names <- list("one", "two", "three")
type <- list("categorical", "numerical", "distributional")
variables <- list(c("cat", "dog"), c(1, 2, 3), NA)
weights <- list(c(0.5,0.5), c(0.3,0.3,0.4), NA)
replace <- list(TRUE, TRUE, TRUE)
distribution_type <- list(NA, NA, "rpois")
distribution_inf <- list(NA, NA, c(4))

df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)
df <- df[,c(2,3)]
y <- 30*df$three + rnorm(1)

continuous_shrinkage(data.frame(df), y, type = "lasso")
continuous_shrinkage(data.frame(df), y, type = "ridge")
continuous_shrinkage(data.frame(df), y, type = "horseshoe")

