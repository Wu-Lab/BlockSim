#' Estiamte the growth rate of blockchain
#'
#'
#' @export
estimate_growth_rate <- function(block_rate, block_size, band_width = 512, N = Inf, log_eps = -1000, gamma_shape = 2)
{
  if (block_rate <= 0) return(0)
  
  delay <- block_size * 1024 * 8 / band_width
  scale <- delay/(gamma_shape-1)
  k <- 10
  p1 <- pgamma((0:(k-1))/block_rate, shape = gamma_shape, scale = scale, lower.tail = F, log.p = T)
  while (k < N && p1[length(p1)] > log_eps)
  {
    p1 <- c(p1, pgamma((k:(2*k-1))/block_rate, shape = gamma_shape, scale = scale, lower.tail = F, log.p = T))
    k <- k * 2
  }
  p0 <- pgamma((1:k)/block_rate, shape = gamma_shape, scale = scale, lower.tail = T, log.p = T)
  if (k >= N)
  {
    k <- N
    p0 <- p0[1:N]
    p1 <- p1[1:N]
    p0[N] <- 0
  }
  s <- (block_rate/(1:k)) * exp(p0 + cumsum(p1))
  sum(s)
}


#' Estimate block distance
#' 
#' 
#' @export
estimate_block_distance <- function(block_sequential_distance, block_rate, growth_rate)
{
  block_sequential_distance * growth_rate / block_rate
}