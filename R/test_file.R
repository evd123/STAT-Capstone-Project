#test_file

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

