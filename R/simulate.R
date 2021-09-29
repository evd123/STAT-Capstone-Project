## SIMULATE FUNCTION ##

simulate <- function(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf){
  columns <- vector()
  for (elem in 1:length(column_names)) {
    v <- vector();
    if (type[elem] == "categorical") {
      v <- catAndNumHelper(n, variables[elem], weights[elem], replace[elem], 0)
    }
    else if (type[elem] == "numerical") {
      v <- catAndNumHelper(n, variables[elem], weights[elem], replace[elem], 1);
    }
    else if (type[elem] == "distributional") {
      v <- distributionalHelper();
    }
    # add v to columns
    columns <- c(columns, v)
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

distributionalHelper <- function(dist, distribution_info) {
  if (dist == "rnorm") {
    if (length(distribution_info) != 3) {
      print("Incorrect number of inputs for this distribution")
    }
    return(rnorm(n = distribution_info[1], mean = distribution_info[2], sd = distribution_info[3]))
  }
  else if (dist == "runif") {
    if (length(distribution_info) != 3) {
      print("Incorrect number of inputs for this distribution")
    }
    return(runif(n = distribution_info[1], min = distribution_info[2], max = distribution_info[3]))
  }
  else if (dist == "rpois") {
    if (length(distribution_info) != 2) {
      print("Incorrect number of inputs for this distribution")
    }
    return(rpois(n = distribution_info[1], lambda = distribution_info[2]))
  }
}



