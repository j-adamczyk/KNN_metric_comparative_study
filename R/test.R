#' Test KNN algorithm using cross-validation
#'
#' Divides provided into parts and performs cross-validation with given parameters.
#'
#' @param dataset data frame or numeric matrix. Cross-validation target.
#' @param parts numeric. Number of parts to divide data into
#' @param predict_param numeric. Index of parameter to predict values of.
#' @param predict_param_type "numeric" or "character". If predict_param values are strings, then choose "character" to save it in predict_set
#' as such.
#' @param k number of nearest neighbours to use for prediction or character string "all". Default: square root of number of
#' observations (rows) of training set.
#' @param metric character string giving a method for computing distances between vectors. One of:
#' "hassanat" (default), "canberra", "chebyshev", "chi_squared", "cosine", "euclidean", "euclidean_normalized", "euclidean_standarized",
#' "hamming", "mahalanobis", "manhattan", "pearson", "spearman".
#' @param weighting_scheme character string giving a method for weighting k nearest neighbours. One of:
#' "inverted" (default), "constant", "inverted_logarythmic".
#'
#' @return percentage of properly predicted values.
#'
#' @export
cross_validate <- function(dataset, parts, predict_param, predict_param_type="numeric", k="all", metric="hassanat", weighting_scheme="inverted")
{
  percentages <- vector(length=parts)
  part_size <- round(nrow(dataset) / parts)
  for (i in 1:parts)
  {
    if (i != parts)
    {
      left <- (i - 1) * part_size + 1
      right<- i * part_size
      predict_set <- dataset[left:right,]
      training_set <- dataset[-(left:right),]
    }
    else
    {
      left <- (i - 1) * part_size + 1
      right <- nrow(dataset)
      predict_set <- dataset[left:right,]
      training_set <- dataset[-(left:right),]
    }
    
    source("./R/main.R")
    predicted_set <- knn(training_set, predict_set, predict_param, predict_param_type, k, metric, weighting_scheme)

    percentages[i] <- round(100 * sum(predict_set[, predict_param] == predicted_set[, predict_param], na.rm=TRUE) / nrow(predict_set), digits=3)
  }
  
  mean(percentages)
}


#' Get best parameter values and precision for dataset
#'
#' Empirically finds best parameter values and KNN precision for given dataset.
#' WARNING: this function may take A LOT of time to finish, especially for large datasets.
#'
#' @param dataset data frame or numeric matrix. Experiment target.
#' @param predict_param numeric. Index of parameter to predict values of.
#' @param predict_param_type "numeric" or "character". If predict_param values are strings, then choose "character" to save it in predict_set
#' as such.
#'
#' @return best precision (percentage) and optimal k, metric and weighting scheme.
#'
#' @export
best_results <- function(dataset, predict_param, predict_param_type="numeric")
{
  # some metrics may cause problems, e. g. Mahalanobis (because of singular covariance matrix); run this function without it if needed
  
  k_vals <- c(as.list(seq(from=1, to=round(sqrt(nrow(dataset))), by=2)), "all")
  metrics <- c("hassanat", "canberra", "chebyshev", "chi_squared", "cosine", "euclidean", "euclidean_normalized", "euclidean_standarized",
               "hamming", "mahalanobis", "manhattan", "pearson", "spearman")
  weighting_schemes <- c("constant", "inverted", "inverted_logarythmic")
  
  best_k <- 0
  best_metric <- ""
  best_weighting_scheme <- ""
  best_prec <- 0
  
  for (k in k_vals)
  {
    for (metric in metrics)
    {
      for (weighting_scheme in weighting_schemes)
      {
        prec <- cross_validate(dataset, 5, predict_param, predict_param_type, k, metric, weighting_scheme)
        if (prec > best_prec)
        {
          best_k <- k
          best_metric <- metric
          best_weighting_scheme <- weighting_scheme
          best_prec <- prec
        }
      }
    }
  }
  
  list(Best_precision=best_prec, k=best_k, metric=best_metric, weighting_scheme=best_weighting_scheme)
}
