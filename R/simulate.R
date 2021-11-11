## SIMULATE FUNCTION ##

list_replicator <- function(n, item) {
  new_list <- list()
  for (i in 1:n) {
    new_list[[i]] = item
  }
  return (new_list)
}

simulate <- function(n = 20,
                     column_names = list("one", "two", "three", "four", "five"),
                     type = as.list(replicate(length(column_names), "numerical")),
                     variables = list_replicator(length(column_names), c(1, 2, 3)),
                     weights = list_replicator(length(column_names), c(0.3, 0.3, 0.4)),
                     replace = as.list(replicate(length(column_names), TRUE)),
                     distribution_type = as.list(replicate(length(column_names), NA)),
                     distribution_inf = as.list(replicate(length(column_names), NA))){
  if (length(column_names) != length(type) || 
      length(column_names) != length(variables) || 
      length(column_names) != length(weights) || 
      length(column_names) != length(replace) ||
      length(column_names) != length(distribution_type) ||
      length(column_names) != length(distribution_inf)) {
    print("There is a missing or extra term in one of the inputs. Please make sure all of the inputs are the same length")
    return()
  }
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
      break
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
  else {
    print("That distribution is not accommodated by this package.")
  }
}


## EXAMPLE (uncomment to use) ##
n <- 20
column_names <- list("one", "two", "three", "four", "five")
type <- list("categorical", "numerical", "distributional", "numerical", "distributional")
variables <- list(c("cat", "dog"), c(1, 2, 3), NA, 20:40, NA)
weights <- list(c(0.5,0.5), c(0.3,0.3,0.4), NA, replicate(21, 1/21), NA)
replace <- list(TRUE, TRUE, TRUE, FALSE, TRUE)
distribution_type <- list(NA, NA, "rpois", NA, "rnorm")
distribution_inf <- list(NA, NA, c(4), NA, c(10, 1))

df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)
df


# todo: 
## add other distributions
## need to fix package outline for specifications
## option to have predictors be correlated (dependent structure) - different levels of correlation; 
  # lin al tools to force correlation structure (automated simulation packages existing); 
  # adding interaction terms

