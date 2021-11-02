## Continuous Shrinkage Prior ##
library(bayesreg)
library(horseshoe)

continous_shrinkage <- function(
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
    horseshoe(response, data,method.tau = "truncatedCauchy")
  }
  else {
    bayesreg(data.frame(data, response), prior = type)
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
y <- 30*df$three

obj <- bayesreg(y~.,data.frame(df,y), prior = "hs")

mean(obj$beta[1,])
mean(obj$beta[2,])


install.packages("horseshoe")
library(horseshoe)
