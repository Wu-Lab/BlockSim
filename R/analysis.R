#' Calculate block connectivity
#'
#'
#' @export
block_connectivity <- function(adj_matrix)
{
  x <- adj_matrix
  for (i in 1:ceiling(log2(dim(x)[1])))
  {
    x <- x + x %*% x
    x[x > 0] <- 1
  }
  past <- rowSums(x)
  future <- colSums(x)
  list(con_mat = x, past = past, future = future)
}
