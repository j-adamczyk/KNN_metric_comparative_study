inv_cov_mat <- matrix()
inv_square_vars <- matrix()

#' Classify target set using K Nearest Neighbours
#'
#' Uses provided training set to predict values of given prediction parameter in target set.
#' The precision of algorithm varies greatly, depending on parameters.
#'
#' @param training_set data frame or numeric matrix - training dataset.
#' @param predict_set data frame or numeric matrix (same as training_set) - prediction dataset.
#' @param predict_param numeric. Index of parameter to predict values of.
#' @param predict_param_type "numeric" or "character". If predict_param values are strings, then choose "character" to save it in predict_set
#' as such.
#' @param k number of nearest neighbours to use for prediction or character string "all".
#' Default: square root of number of observations in training_set.
#' @param metric character string giving a method for computing distances between vectors. One of:
#' "hassanat" (default), "canberra", "chebyshev", "chi_squared", "cosine", "euclidean", "euclidean_normalized", "euclidean_standarized",
#' "hamming", "mahalanobis", "manhattan", "pearson", "spearman".
#' @param weighting_scheme character string giving a method for weighting k nearest neighbours. One of:
#' "inverted" (default), "constant", "inverted_logarythmic".
#'
#' @return predict_set with predicted values of predict_param (with same column type as in training_set).
#'
#' @export
knn <- function(training_set, predict_set, predict_param, predict_param_type="numeric", k=round(sqrt(nrow(training_set))),
                metric="hassanat", weighting_scheme="inverted")
{
  # check arguments correctness and prepare them
  if (is.data.frame(training_set))
  {
    original_training_set <- training_set
    training_set <- data.matrix(training_set)
    training_set_no_predict_param <- training_set[,-predict_param]
  }
  else
  {
    if (!is.matrix(training_set))
      stop("training_set must be data frame or matrix")
  }
  
  if (is.data.frame(predict_set))
  {
    original_predict_set <- predict_set
    predict_set <- data.matrix(predict_set)
    predict_set_no_predict_param <- predict_set[,-predict_param]
  }
  else
  {
    if (!is.matrix(predict_set))
      stop("predict_set must be data frame or matrix")
  }
  
  if (ncol(training_set) != ncol(predict_set))
    stop("ncol of training_set and predict_set must be the same")
  
  if (predict_param < 1 || predict_param > ncol(training_set))
    stop("predict_param must be between 1 and column number of training set")
  
  if (is.numeric(k) && k < 1)
  { stop("k must be >= 1") }
  else if (k == "all")
  { k <- nrow(training_set) }
  else if (is.numeric(k))
  { k <- min(k, ncol(training_set)) }
  else
  { stop('k must be one of: numeric, "all", "best_fit"') }
  
  source("./R/metrics.R")
  if (metric == "mahalanobis")
  { inv_cov_mat <<- solve(cov(training_set_no_predict_param)) }
  else if (metric == "euclidean_standarized")
  {
    vars <- apply(training_set_no_predict_param, 2, var)
    inv_square_vars <<- 1 / (vars)^2
  }

  metric <- match.fun(metric)
  
  source("./R/weighting_schemes.R")
  weighting_scheme <- match.fun(weighting_scheme)
  
  # predict value of predict_param for each observation in predict_set
  for(i in 1:nrow(predict_set))
  {
    curr_vec <- predict_set_no_predict_param[i,]

    # calculate distances between current row vector from predict_set and every row vector from training_set
    distances <- apply(training_set_no_predict_param, 1, metric, vec2=curr_vec)
    
    # gets indices of k nearest neighbors
    indices <- order(distances)[1:k]
    
    # get neighbours' prediction parameter values
    vals <- training_set[indices, predict_param]

    # weight
    vals <- weighting_scheme(vals)
    
    # predict value
    predict_set[i, predict_param] = round(mean(vals))
  }
  
  # convert predicted numeric values to character if needed
  if (predict_param_type == "character")
  {
    fac <- factor(original_training_set[, predict_param])
    predict_set[, predict_param] <- levels(fac)[predict_set[, predict_param]]
  }

  predict_set
}
