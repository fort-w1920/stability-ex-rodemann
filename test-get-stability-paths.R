# ---- test-get-stability-paths ----

library(MASS)

data(prostate, package = "ElemStatLearn")
data <- prostate

max_formula <- lpsa ~ (. - train)
model <- leaps::regsubsets(max_formula,
                    data = data, nbest = 1, nvmax = 8,
                    really.big = TRUE
)

set.seed(20141020)
stability_paths <- get_stability_paths(model, data, reps = 1000)
stability_paths
