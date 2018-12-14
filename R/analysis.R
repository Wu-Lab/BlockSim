#' Find faithful blocks
#'
#' @import igraph
#'
#' @export
find_faithful_blocks <- function(adj_matrix, max_discord = 10)
{
  d <- block_discords(adj_matrix)
  d[d <= max_discord] <- 0
  d[d > max_discord] <- 1
  largest_ivs(graph_from_adjacency_matrix(d, mode = "undirected"))[[1]]
}


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
  d <- apply(x[colSums(adj_matrix) == 0, , drop = F], 2, min)
  past <- lapply(1:n, function(i) x[i, ] < Inf)
  future <- lapply(1:n, function(i) x[, i] < Inf)
  for (i in 2:n)
  {
    for (j in 1:(i-1))
    {
      if (x[i, j] == Inf && x[j, i] == Inf)
      {
        p <- past[[i]] & past[[j]]
        f <- future[[i]] & future[[j]]
        discords[i, j] <- discords[j, i] <- min(x[i, p] + x[j, p]) + min(x[f, i] + x[f, j], d[i] + d[j] + 2)
      }
    }
  }
  discords
}


#' Calculate the length of main chain
#' 
#' 
#' @import igraph
#' 
#' @export
length_of_main_chain <- function(adj_matrix)
{
  g <- graph_from_edgelist(which(adj_matrix == 1, arr.ind = T), directed = T)
  edge_attr(g, "weight") <- -1
  d <- distances(g, 1, mode = "in", algorithm = "bellman-ford")
  - min(d)
}


#' Calculate the transactions per block
#' 
#' 
#' @export
tx_per_block <- function(block_size = 1)
{
  block_size * 1024^2 / 500
}


#' Calculate the transactions per second
#' 
#' 
#' @export
tx_per_second <- function(effective_block_rate, block_size = 1)
{
  tx_per_block(block_size) * effective_block_rate
}
