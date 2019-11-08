## extract stability paths from refits of <model> on resampled <data>.
## arguments:
##    model: a fitted model object of class "regsubsets"
##    data: the data used for the model fit
##    method: resampling method, either "subsample" (without replacement) or
##      "bootstrap" (with replacement)
##    strata: for subsampling, vector (length = nrow(data)) defining the strata
##      for stratified sampling.
##    fraction: subsampling fraction
## return: a <max. subsetsize + 1> x <covariates> matrix with relative selection
##    frequencies. first row is the null model (no covariates,i.e., all 0s)
## dependencies: {checkmate}, {leaps}
get_stability_paths <- function(model, data, reps = 100,
                                method = c("subsample", "bootstrap"),
                                strata = NULL, fraction = 0.5) {
  checkmate::assert_class(model, "regsubsets")
  checkmate::assert_data_frame(data)
  checkmate::assert_count(reps)
  method <- match.arg(method)
  checkmate::assert_vector(strata, any.missing = FALSE,
                           len = NROW(data), null.ok = TRUE)
  checkmate::assert_number(fraction, lower = 0, upper = 1)

  selected <- list()
  for (i in seq_len(reps)) {
    new_data <- resample(data, method = method, strata = strata,
                         fraction = fraction)
    new_model <- refit(model, new_data)
    selected[[i]] <- get_selected(new_model)
  }
  stability_paths <- make_paths(selected)
  stability_paths
}

############## resample ########################################################

resample <- function(data, method = c("subsample", "bootstrap"),
                     strata = NULL, fraction = 0.5) {
  nrows <- nrow(data)
  rows <- resample_rows(nrows, method, strata, fraction)
  data[rows, ]
}

resample_rows <- function(nrows, method, strata = NULL, fraction = 0.5) {
  switch(method,
         "bootstrap" = sample_with_replacement(nrows, strata),
         "subsample" = sample_without_replacement(nrows, strata,
                                                  fraction = fraction)
  )
}

sample_with_replacement <- function(nrows, strata = NULL) {
  if (is.null(strata)) {
    return(sample(nrows, replace = TRUE)) # --> early exit!
  }
  rows <- tapply(
    X = seq_len(nrows), INDEX = strata, FUN = sample, replace = TRUE
  )
  as.vector(rows)
}

############## refit ###########################################################

# redo subset selection on new_data.
refit <- function(model, new_data) {
  # works by overwriting the data argument of the original model
  # and then re-doing the function call that produced the original model
  modelcall <- model$call
  modelcall$data <- new_data
  # use regsubsets-generic here instead of regsubsets.formula or other method as
  # these methods are not exported by {leaps}
  # (quote s.t. just the name of the function is handed over, not the
  # function code itself...)
  modelcall[[1]] <- quote(leaps::regsubsets)
  eval(modelcall)
}
