#' Calculate shortest distances of unweighted graph
#' 
#' Calculate all pairs of shortest distances of unweighted graph
#' 
#' This function calculates all pairs of shortest distances of unweighted graph
#' by using breadth-first-search (BFS) algorithm.
#' 
#' @param adj_matrix Logical adjacency matrix of given unweighted graph
#' @param source_nodes Logical vector to indicate the source nodes that 
#' need to calculate the shortest distances
#' @return This function will return the shortest distance matrix, where the element
#' \code{[i, j]} is the shortest distance between node i and j. Value Inf means unreachable.
#' If \code{source_nodes[i]} equals FALSE, the shortest distance from i to other nodes
#' will not be calculated and the row i will be all Inf.
#' 
#' @import Matrix
#' 
#' @export
get_shortest_distances <- function(adj_matrix, source_nodes = rep_len(TRUE, dim(adj_matrix)[1]))
{
  edges <- which(adj_matrix != 0, arr.ind = TRUE)
  edges <- edges[order(edges[,1]), ]
  index <- findInterval(0:dim(adj_matrix)[1], edges[,1])
  dist <- .Call(BC_ShortestDistances, edges, index, source_nodes)
  dist[dist == -1] <- Inf
  dist
}
