#' Find the complementary set
#' 
#' 
#' @param n The size of complete set
#' @param v The vector containing a subset of 1:n
#' 
#' @return This function will return the complementary set of a given subset.
#' 
#' @export
complement <- function(n, v)
{
  x <- rep_len(T, n)
  x[v] <- F
  which(x)
}
