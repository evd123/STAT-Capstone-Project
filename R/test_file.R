#test_file

## EXAMPLE (uncomment to use) ##
n <- 20
column_names <- list("one", "two", "three", "four", "five")
type <- list("categorical", "numerical", "distributional", "numerical", "distributional")
variables <- list(c("cat", "dog"), c(1, 2, 3), NA, 20:40, NA)
weights <- list(c(0.3,0.7), c(0.3,0.3,0.4), NA, replicate(21, 1/21), NA)
replace <- list(TRUE, TRUE, TRUE, FALSE, TRUE)
distribution_type <- list(NA, NA, "rpois", NA, "rnorm")
distribution_inf <- list(NA, NA, c(4), NA, c(10, 1))

df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)
df



### EXAMPLE 1 ###


set.seed(100)

n <- 20
column_names <- list("one", "two", "three")
type <- list("numerical", "numerical", "distributional")
variables <- list(c(10:100), c(1, 2, 3), NA)
weights <- list(rep(1/91, 91), c(0.3,0.3,0.4), NA)
replace <- list(TRUE, TRUE, TRUE)
distribution_type <- list(NA, NA, "rpois")
distribution_inf <- list(NA, NA, c(4))

df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)

y <- 20*df$one + 10*df$three



bayes_factor(data = df, response = y, no_prior_information = TRUE)
bayes_factor(data = df, response = y, desired_sparsity = 0.2, desired_prior_effect=0.6)
# Effects of Prior Probabilities
bayes_factor(data = df, response = y, covariate_probabilities = c(0.9, 0.1,0.9,0.1))
bayes_factor(data = df, response = y, covariate_probabilities = c(0.1, 0.9,0.1,0.9))


### EXAMPLE 2 ###


n <- 20
column_names <- list("one", "two", "three")
type <- list("numerical", "numerical", "distributional")
variables <- list(10:20, c(1, 2, 3), NA)
weights <- list(rep(1/11,11), c(0.3,0.3,0.4), NA)
replace <- list(TRUE, TRUE, TRUE)
distribution_type <- list(NA, NA, "rpois")
distribution_inf <- list(NA, NA, c(4))

df <- simulate(n, column_names, type, variables, weights, replace, distribution_type, distribution_inf)

y <- 5*df$two + 10*df$three

bayes_factor(data = df, response = y, no_prior_information = TRUE)
bayes_factor(data = df, response = y, desired_sparsity = 0.2, desired_prior_effect=0.6)
# Effects of Prior Probabilities
bayes_factor(data = df, response = y, covariate_probabilities = c(0.1, 0.9,0.9,0.1))
bayes_factor(data = df, response = y, covariate_probabilities = c(0.9, 0.1,0.1,0.9))
