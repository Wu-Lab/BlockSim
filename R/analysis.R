#' Calculate block distances
#'
#'
#' @export
block_distances <- function(adj_matrix)
{
  x <- shortest_distances(adj_matrix)
  xx <- x > 0 & x < Inf
  past <- rowSums(xx)
  future <- colSums(xx)
  list(distances = x, past = past, future = future)
}


#' Calculate block discords
#'
#'
#' @export
block_discords <- function(adj_matrix)
{
  n <- dim(adj_matrix)[1]
  discords <- matrix(0, n, n)
  x <- shortest_distances(adj_matrix)
  tips <- colSums(adj_matrix) == 0
  for (i in 2:n)
  {
    for (j in 1:(i-1))
    {
      if (x[i, j] == Inf)
        discords[i, j] <- discords[j, i] <- min(x[i, ] + x[j, ]) + min(x[, i] + x[, j], min(x[tips, i]) + min(x[tips, j]) + 2)
    }
  }
  discords
}
  