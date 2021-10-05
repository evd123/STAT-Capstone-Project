## SIMULATE FUNCTION ##

simulate <- function(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf){
  columns <- c()
  for (elem in 1:length(column_names)) {
    v <- vector();
    if (type[[elem]] == "categorical") {
      v <- catAndNumHelper(n, variables[[elem]], weights[[elem]], replace[[elem]], 0)
    }
    else if (type[[elem]] == "numerical") {
      v <- catAndNumHelper(n, variables[[elem]], weights[[elem]], replace[[elem]], 1);
    }
    else if (type[[elem]] == "distributional") {
      v <- distributionalHelper(n, distribution_type[[elem]], distribution_inf[[elem]]);
    }
    # add v to columns
    if (length(columns) == 0) {
      columns <- data.frame(v)
    } else {
      columns <- cbind.data.frame(columns, v)
    }
  }
  df <- data.frame(columns)
  colnames(df) <- column_names
  return(df)
}

catAndNumHelper <- function(n, values, weights, replace, type){
  if (type == 0) {
    return(sample(x = as.factor(values), size = n, replace = replace, prob = weights))
  }
  else if (type == 1) {
    return(sample(x = values, size = n, replace = replace, prob = weights))
  }
}

distributionalHelper <- function(n, dist, distribution_info) {
  if (dist == "rnorm") {
    if (length(distribution_info) != 2) {
      print("Incorrect number of inputs for this distribution")
    }
    return(rnorm(n = n, mean = distribution_info[[1]], sd = distribution_info[[2]]))
  }
  else if (dist == "runif") {
    if (length(distribution_info) != 2) {
      print("Incorrect number of inputs for this distribution")
    }
    return(runif(n = n, min = distribution_info[[1]], max = distribution_info[[2]]))
  }
  else if (dist == "rpois") {
    if (length(distribution_info) != 1) {
      print("Incorrect number of inputs for this distribution")
    }
    return(rpois(n = n, lambda = distribution_info[[1]]))
  }
}


## EXAMPLE (uncomment to use) ##
# n <- 20
# column_names <- list("one", "two", "three")
# type <- list("categorical", "numerical", "distributional")
# variables <- list(c("cat", "dog"), c(1, 2, 3), NA)
# weights <- list(c(0.5,0.5), c(0.3,0.3,0.4), NA)
# replace <- list(TRUE, TRUE, TRUE)
# distribution_type <- list(NA, NA, "rpois")
# distribution_inf <- list(NA, NA, c(4))
# 
# df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)




# todo: 
## add other distributions
## need to fix package outline for specifications

