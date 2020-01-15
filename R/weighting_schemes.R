constant <- function(vals)
  vals

inverted <- function(vals)
{
  weights <- 1 / seq(length(vals))
  weighted.mean(vals, weights)
}

inverted_logarythmic <- function(vals)
{
  weights <- 1 / log2(1 + seq(length(vals)))
  weighted.mean(vals, weights)
}
