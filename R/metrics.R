canberra <- function(vec1, vec2)
  sum(abs(vec1 - vec2) / (abs(vec1) + abs(vec2)))

chebyshev <- function(vec1, vec2)
  max(vec1 - vec2)

chi_squared <- function(vec1, vec2)
  sum((vec1 - vec2) * (vec1 - vec2) / (vec1 + vec2))

cosine <- function(vec1, vec2)
  1 - sum(vec1 * vec2) / (sqrt(sum(vec1 * vec1)) + sqrt(sum(vec2 * vec2)))

euclidean <- function(vec1, vec2)
  sqrt(sum((vec1 - vec2) * (vec1 - vec2)))

euclidean_normalized <- function(vec1, vec2)
  sqrt(sum((vec1 - vec2) * (vec1 - vec2))) / sqrt(sum(vec1 * vec1))

euclidean_standarized <- function(vec1, vec2)
  sqrt(sum((vec1 - vec2) * (vec1 - vec2) * inv_square_vars))
  
hamming <- function(vec1, vec2)
  sum(vec1 != vec2)

hassanat <- function(vec1, vec2)
{
  sum <- 0
  for (i in 1:length(vec1))
  {
    x <- vec1[i]
    y <- vec2[i]
    min_xy <- min(x, y)
    max_xy <- max(x, y)
    if (min_xy >= 0)
    { sum <- sum + 1 - (1 + min_xy)/(1 + max_xy) }
    else
    { sum <- sum + 1 - (1 + min_xy + abs(min_xy))/(1 + max_xy + abs(min_xy)) }
  }
  sum
}

mahalanobis <- function(vec1, vec2)
{
  get("inv_cov_mat", envir=parent.frame(), inherits=TRUE)
  diff <- vec1 - vec2
  sqrt(t(diff) %*% inv_cov_mat %*% diff)
}

manhattan <- function(vec1, vec2)
  sum(abs(vec1 - vec2))

pearson <- function(vec1, vec2)
  1 - abs(cor(vec1, vec2))

spearman <- function(vec1, vec2)
  1 - abs(cor(vec1, vec2, method="spearman"))
